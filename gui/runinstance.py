#
#    Copyright (C) 2012 Stanislav Bohm
#
#    This file is part of Kaira.
#
#    Kaira is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, version 3 of the License, or
#    (at your option) any later version.
#
#    Kaira is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with Kaira.  If not, see <http://www.gnu.org/licenses/>.
#

import utils
from copy import copy
from drawing import VisualConfig

class RunInstance:

    def __init__(self, project, process_count, threads_count):
        self.project = project
        self.process_count = process_count
        self.threads_count = threads_count

        self.instance_groups = {}
        self.activites = [None] * (self.process_count * self.threads_count)
        self.last_event = None # "fired" / "finished" / None
        self.last_event_activity = None
        self.last_event_instance = None

    def get_instance_groups(self):
        return self.instance_groups.values()

    def add_token(self, place_id, token_pointer, token_value):
        self.last_event_instance.add_token(place_id, token_pointer, token_value)

    def remove_token(self, place_id, token_pointer):
        self.last_event_instance.remove_token(place_id, token_pointer)

    def add_enabled_transition(self, transition_id):
        self.last_event_instance.add_enabled_transition(transition_id)

    def set_activity(self, process_id, thread_id, activity):
        index = process_id * self.threads_count + thread_id
        self.activites[index] = activity
        self.last_event_activity = activity

    def event_spawn(self, process_id, thread_id, time, net_id, group_id, parent_id):
        self.last_event = "spawn"
        if thread_id is not None:
            self.set_activity(process_id, thread_id, None)

        group = self.instance_groups.get(group_id)

        if group is None:
            net = self.project.find_net(net_id)
            assert net is not None
            parent = self.project.find_net(parent_id)
            group = NetInstanceGroup(self, group_id, net, parent)
            self.instance_groups[group_id] = group
        instance = NetInstance(process_id)
        group.add_net_instance(instance)
        self.last_event_instance = instance

    def event_receive(self, process_id, thread_id, group_id):
        self.last_event = "receive"
        self.last_event_instance = self.instance_groups[group_id].net_instances[process_id]
        self.set_activity(process_id, thread_id, None)

    def transition_fired(self, time, process_id, thread_id, group_id, transition_id):
        self.last_event = "fired"
        group = self.instance_groups[group_id]
        self.last_event_instance = group.net_instances[process_id]
        transition = group.net.item_by_id(transition_id)
        self.last_event_activity = \
            TransitionExecution(time, process_id, thread_id, group_id, transition)
        for place in transition.get_packing_input_places():
            self.last_event_instance.remove_all_tokens(place.id)
        if transition.has_code():
            self.activites[process_id * self.threads_count + thread_id] = self.last_event_activity

    def transition_finished(self, time, process_id, thread_id):
        self.last_event = "finish"
        self.last_event_process = process_id
        self.last_event_thread = thread_id
        index = process_id * self.threads_count + thread_id
        self.last_event_activity = self.activites[index]
        group = self.instance_groups[self.last_event_activity.group_id]
        self.last_event_instance = group.net_instances[process_id]
        self.activites[index] = None

    def copy(self):
        runinstance = RunInstance(self.project,
                                  self.process_count,
                                  self.threads_count)
        for i in self.instance_groups:
            group = self.instance_groups[i].copy()
            group.run_instance = runinstance
            runinstance.instance_groups[i] = group

        runinstance.activites = self.activites[:]
        return runinstance

class NetInstanceGroup:

    def __init__(self, run_instance, id, net, parent):
        self.run_instance = run_instance
        self.id = id
        self.net = net
        self.parent = parent
        self.net_instances = {} # Instances according processes

    def add_net_instance(self, net_instance):
        self.net_instances[net_instance.process_id] = net_instance

    def get_name(self):
        return self.net.name

    def get_perspectives(self):
        perspectives = [ Perspective("All", self.run_instance, self, self.net_instances) ]
        v = self.net_instances.keys()
        v.sort()
        for i in v:
            perspectives.append(
                Perspective(str(i),
                self.run_instance, self, { i : self.net_instances[i] } ))
        return perspectives

    def copy(self):
        group = NetInstanceGroup(self.run_instance, self.id, self.net, self.parent)
        for i in self.net_processes:
            group.net_instances[i] = self.net_instances[i].copy()
        return group

class ThreadActivity:

    def __init__(self, time, process_id, thread_id):
        self.time = time
        self.process_id = process_id
        self.thread_id = thread_id

class TransitionExecution(ThreadActivity):

    def __init__(self, time, process_id, thread_id, group_id, transition):
        ThreadActivity.__init__(self, time, process_id, thread_id)
        self.group_id = group_id
        self.transition = transition

class NetInstance:

    def __init__(self, process_id, tokens=None):
        self.process_id = process_id
        self.enabled_transitions = None
        if tokens is None:
            self.tokens = {}
        else:
            self.tokens = tokens

    def add_token(self, place_id, token_pointer, token_value):
        lst = self.tokens.get(place_id)
        if lst is None:
            lst = []
            self.tokens[place_id] = lst
        lst.append((token_pointer, token_value))

    def remove_token(self, place_id, token_pointer):
        lst = self.tokens.get(place_id)
        if lst is None:
            return
        for i in xrange(len(lst)):
            if lst[i][0] == token_pointer:
                del lst[i]
                return

    def remove_all_tokens(self, place_id):
        self.tokens[place_id] = None

    def add_enabled_transition(self, transition_id):
        if self.enabled_transitions is None:
            self.enabled_transitions = []
        self.enabled_transitions.append(transition_id)

    def copy(self):
        netinstance = NetInstance(self.process_id, copy(self.tokens))
        netinstance.enabled_transitions = copy(self.enabled_transitions)
        return netinstance


class NetInstanceVisualConfig(VisualConfig):

    def __init__(self, transition_executions, enabled_transitions, tokens):
        # transition_id -> [ text_labels ]
        self.transition_executions = transition_executions
        self.tokens = tokens
        self.enabled_transitions = enabled_transitions

    def transition_drawing(self, item):
        drawing = VisualConfig.transition_drawing(self, item)
        executions = self.transition_executions.get(item.id)
        drawing.executions = executions
        if item.id in self.enabled_transitions:
            drawing.highlight = (0, 1, 0)
        return drawing

    def place_drawing(self, item):
        drawing = VisualConfig.place_drawing(self, item)
        drawing.set_tokens(self.tokens[item.id])
        return drawing


class Perspective(utils.EqMixin):

    def __init__(self, name, run_instance, group, net_instances):
        self.name = name
        self.run_instance = run_instance
        self.group = group
        self.net_instances = net_instances

    def get_tokens(self, place):
        tokens = []
        for net_instance in self.net_instances.values():
            t = net_instance.tokens.get(place.id)
            if t is not None:
                for token_pointer, token_value in t:
                    tokens.append("{0}@{1}".format(token_value, net_instance.process_id))
        return tokens

    def get_visual_config(self):
        activies_by_transitions = {}
        for tr in self.group.net.transitions():
            activies_by_transitions[tr.id] = []

        color = (1.0, 1.0, 0, 0.8)
        run_instance = self.run_instance
        for i in range(run_instance.threads_count * run_instance.process_count):
            activity = run_instance.activites[i]
            if isinstance(activity, TransitionExecution) \
                and activity.transition.id in activies_by_transitions:
                
                if activity != run_instance.last_event_activity:
                    activies_by_transitions[activity.transition.id].append((activity, color))

        if (run_instance.last_event == "fired" or run_instance.last_event == "finish") \
            and run_instance.last_event_activity.transition.id \
                in activies_by_transitions:
            color = (0, 1, 0, 0.8) if run_instance.last_event == "fired" else (1, 0, 0, 0.8)
            activies_by_transitions[run_instance.last_event_activity.transition.id].append(
                (run_instance.last_event_activity, color))

        transition_executions = {}
        for transition_id, lst in activies_by_transitions.items():
            lst.sort(key=lambda pair: pair[0].time) # Sort by time
            transition_executions[transition_id] = [
                ("{0.process_id}/{0.thread_id}".format(activity), color)
                for activity, color in lst ]

        tokens = {}
        for place in self.group.net.places():
            tokens[place.id] = self.get_tokens(place)

        enabled = set()
        enabled.update(*[ net_instance.enabled_transitions
                          for net_instance in self.net_instances.values()
                          if net_instance.enabled_transitions is not None ])

        return NetInstanceVisualConfig(transition_executions, enabled, tokens)
