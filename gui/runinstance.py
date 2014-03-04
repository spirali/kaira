#
#    Copyright (C) 2012-2013 Stanislav Bohm
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


class Packet:

    def __init__(self, time, size, edge_id):
        self.time = time
        self.size = size
        self.edge_id = edge_id


class RunInstance:

    def __init__(self, project, process_count):
        self.project = project
        self.process_count = process_count
        self.net = None
        self.net_instances = {}
        self.activites = [None] * self.process_count
        self.last_event = None # "fire" / "finished" / "receive" / None
        self.last_event_activity = None
        self.last_event_instance = None
        self.last_event_time = None
        self.packets = [ [] for i in xrange(self.process_count * self.process_count)]

    def add_token(self, place_id, token_pointer, token_value, send_time=None):
        self.last_event_instance.add_token(place_id, token_pointer, token_value, send_time)

    def remove_token(self, place_id, token_pointer):
        self.last_event_instance.remove_token(place_id, token_pointer)

    def clear_removed_and_new_tokens(self):
        for i in self.net_instances:
            self.net_instances[i].clear_removed_and_new_tokens()

    def add_enabled_transition(self, transition_id):
        self.last_event_instance.add_enabled_transition(transition_id)

    def set_activity(self, process_id, activity):
        self.activites[process_id] = activity
        self.last_event_activity = activity

    def pre_event(self):
        """ This method is called by tracelog before each event_* """
        self.clear_removed_and_new_tokens()

    def reset_last_event_info(self):
        self.last_event = None
        self.last_event_activity = None
        self.last_event_instance = None
        self.last_event_time = None

    def event_spawn(self, process_id, time, net_id):
        self.net = self.project.find_net(net_id)
        assert self.net.id == net_id
        self.last_event = "spawn"
        self.set_activity(process_id, None)
        instance = NetInstance(process_id)
        self.net_instances[process_id] = instance
        self.last_event_instance = instance
        self.last_event_process = process_id
        self.last_event_time = time

    def event_quit(self, process_id, time):
        self.last_event = "quit"
        self.last_event_process = process_id
        self.last_event_time = time
        self.last_event_activity = self.activites[process_id]
        if self.last_event_activity is not None:
            # None can occur when we are logging
            # "quit" but not transition fire
            self.last_event_activity.quit = True
        self.last_event_instance = self.net_instances[process_id]

    def event_idle(self, process_id, time):
        self.last_event = "idle"
        self.last_event_process = process_id
        self.last_event_time = time
        self.last_event_activity = None
        self.last_event_instance = self.net_instances[process_id]

    def event_send(self, process_id, time, target_id, size, edge_id):
        packet = Packet(time, size, edge_id)
        self.packets[target_id * self.process_count + process_id].append(packet)

    def event_end(self, process_id, time):
        pass

    def event_receive(self, process_id, time, origin_id):
        self.last_event = "receive"
        self.last_event_process = process_id
        self.last_event_time = time
        packets = self.packets[process_id * self.process_count + origin_id]
        packet = packets[0]
        del packets[0]
        self.last_event_instance = self.net_instances[process_id]
        self.set_activity(process_id,
                          Receive(time, process_id, origin_id))
        return time - packet.time

    def transition_fired(self, process_id, time, transition_id, values):
        self.last_event = "fire"
        self.last_event_instance = self.net_instances[process_id]
        self.last_event_process = process_id
        self.last_event_time = time
        transition = self.net.item_by_id(transition_id)
        self.last_event_activity = \
            TransitionFire(time, process_id, transition, values)
        if transition.has_code() or transition.collective:
            self.activites[process_id] = self.last_event_activity

    def transition_blocked(self, process_id):
        self.activites[process_id].blocked = True

    def transition_finished(self, process_id, time):
        self.last_event = "finish"
        self.last_event_process = process_id
        self.last_event_time = time
        self.last_event_activity = self.activites[process_id]
        self.last_event_instance = self.net_instances[process_id]
        self.activites[process_id] = None

    def copy(self):
        runinstance = RunInstance(self.project,
                                  self.process_count)
        for i in self.net_instances:
            n = self.net_instances[i].copy()
            runinstance.net_instances[i] = n

        runinstance.activites = self.activites[:]
        return runinstance

    def get_perspectives(self):
        perspectives = [ Perspective("All", self, self.net_instances) ]
        v = self.net_instances.keys()
        v.sort()
        for i in v:
            perspectives.append(
                Perspective(str(i),
                self, { i : self.net_instances[i] } ))
        return perspectives

    def get_packets_info(self, edge_id, process_id):
        results = []
        for i in xrange(self.process_count):
            packets = self.packets[process_id * self.process_count + i]
            ps = [ p for p in packets if p.edge_id == edge_id ]
            if ps:
                itr = iter(ps)
                first = next(itr)
                text = "{0} -> {1} | {2}".format(i, process_id, first.size)
                if len(ps) > 1:
                    size = 0
                    for p in itr:
                        size += p.size
                    text += " ({0}, {1})".format(len(ps) - 1, size)
                if packets[0].edge_id != edge_id:
                    top = False
                    text += " *"
                else:
                    top = True
                results.append((process_id, i, top, text))
        return results

    def get_packets_count(self, origin_id, target_id):
        return len(self.packets[target_id * self.process_count + origin_id])


class ProcessActivity:

    def __init__(self, time, process_id):
        self.time = time
        self.process_id = process_id

class TransitionFire(ProcessActivity):

    name = "fire"
    quit = False
    blocked = False

    def __init__(self, time, process_id, transition, values):
        ProcessActivity.__init__(self, time, process_id)
        self.transition = transition
        self.values = values


class Receive(ProcessActivity):

    name = "receive"

    def __init__(self, time, process_id, origin_id):
        ProcessActivity.__init__(self, time, process_id)
        self.origin_id = origin_id


class NetInstance:

    def __init__(self, process_id, tokens=None):
        self.process_id = process_id
        self.enabled_transitions = None
        self.new_tokens = {}
        self.removed_tokens = {}
        if tokens is None:
            self.tokens = {}
        else:
            self.tokens = tokens

    def add_token(self, place_id, token_pointer, token_value, send_time):
        lst = self.new_tokens.get(place_id)
        if lst is None:
            lst = []
            self.new_tokens[place_id] = lst

        if isinstance(token_value, list):
            if not token_value: # the list of tokens is empty
                token_value = None
            elif len(token_value) == 1:
                token_value = token_value[0]
        lst.append((token_pointer, token_value, send_time))

    def clear_removed_and_new_tokens(self):
        """
            'new_tokens' are moved into regular list of tokens and
            'removed_tokens' tokens are emptied
        """
        if self.new_tokens:
            for place_id in self.new_tokens:
                lst = self.tokens.get(place_id)
                if lst is None:
                    lst = []
                    self.tokens[place_id] = lst
                lst += self.new_tokens.get(place_id)
            self.new_tokens = {}

        if self.removed_tokens:
            self.removed_tokens = {}

    def remove_token(self, place_id, token_pointer):
        lst = self.tokens.get(place_id)
        if lst is None:
            return

        removed_lst = self.removed_tokens.get(place_id)
        if removed_lst is None:
            removed_lst = []
            self.removed_tokens[place_id] = removed_lst

        for i in xrange(len(lst)):
            if lst[i][0] == token_pointer:
                removed_lst.append(lst[i])
                del lst[i]
                return

    def remove_all_tokens(self, place_id):
        self.removed_tokens[place_id] = self.tokens.get(place_id)
        self.tokens[place_id] = None

    def add_enabled_transition(self, transition_id):
        if self.enabled_transitions is None:
            self.enabled_transitions = []
        self.enabled_transitions.append(transition_id)

    def copy(self):
        netinstance = NetInstance(self.process_id, copy(self.tokens))
        netinstance.enabled_transitions = copy(self.enabled_transitions)
        return netinstance


class Perspective(utils.EqMixin):

    ARROW_LEFT  = u"\u21a4 "
    ARROW_RIGHT = u"\u21a6 "
    BULLET = u"\u2022 "

    def __init__(self, name, runinstance, net_instances):
        self.name = name
        self.runinstance = runinstance
        self.net_instances = net_instances

    def get_tokens(self, place):
        tokens = []
        for net_instance in self.net_instances.values():
            t = net_instance.tokens.get(place.id)
            if t is not None:
                for token_pointer, token_value, token_time in t:
                    if token_value is None:
                        token_value = Perspective.BULLET
                    tokens.append("{0}@{1}".format(token_value, net_instance.process_id))
        return tokens

    def get_new_tokens(self, place):
        tokens = []
        for net_instance in self.net_instances.values():
            t = net_instance.new_tokens.get(place.id)
            if t is not None:
                for token_pointer, token_value, token_time in t:
                    if token_value is None:
                        token_value = Perspective.ARROW_LEFT

                    if token_time:
                        tokens.append("{0}@{1} ({2})".format(
                            token_value,
                            net_instance.process_id,
                            utils.time_to_string(token_time, seconds=True)))
                    else:
                        tokens.append("{0}@{1}".format(token_value, net_instance.process_id))
        return tokens

    def get_packets_info(self, edge_id):
        results = []
        for net_instance in self.net_instances.values():
            results += self.runinstance.get_packets_info(edge_id, net_instance.process_id)
        return results

    def get_removed_tokens(self, place):
        tokens = []
        for net_instance in self.net_instances.values():
            t = net_instance.removed_tokens.get(place.id)
            if t is not None:
                for token_pointer, token_value, token_time in t:
                    if token_value is None:
                        token_value = Perspective.ARROW_RIGHT

                    tokens.append("{0}@{1}".format(token_value, net_instance.process_id))
        return tokens

    def get_transition_trace_values(self, transition):
        if self.runinstance.net is None:
            return None

        values = []
        runinstance = self.runinstance
        for i in range(runinstance.process_count):
            activity = runinstance.activites[i]
            if isinstance(activity, TransitionFire) \
                and activity.transition.id == transition.id:
                    s = "{0} -> ".format(i)
                    values.append(s + "; ".join(map(str, activity.values)) + ";")
        return values

    def get_enabled_transitions(self):
        enabled = set()
        enabled.update(*[ net_instance.enabled_transitions
                          for net_instance in self.net_instances.values()
                          if net_instance.enabled_transitions is not None ])
        return enabled

    def is_transition_enabled(self, transition):
        return transition.id in self.get_enabled_transitions()


    def get_activations_values(self, transition):
        runinstance = self.runinstance
        result = []
        for p in xrange(runinstance.process_count):
            activity = runinstance.activites[p]
            if (runinstance.last_event_activity and
                runinstance.last_event_activity.name == "fire" and
                runinstance.last_event_activity.transition == transition and
                runinstance.last_event_activity.process_id == p):
                    if runinstance.last_event == "fire":
                        color = (0, 1, 0, 0.8)
                    elif runinstance.last_event_activity.quit:
                        color = (0.45, 0.45, 0.45, 0.8)
                    else:
                        color = (1, 0, 0, 0.8)
                    activity = runinstance.last_event_activity
            elif isinstance(activity, TransitionFire) and \
                    activity.transition == transition:
                if activity.blocked:
                    color = (0.75, 0.75, 0.75, 0.9)
                else:
                    color = (1.0, 1.0, 0.0, 0.8)
            else:
                continue
            result.append((str(activity.process_id), color, (p, transition)))
        return result

    def get_process_ids(self):
       return [ net_instance.process_id
                for net_instance in self.net_instances.values() ]
