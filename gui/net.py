#
#    Copyright (C) 2010-2014 Stanislav Bohm
#                  2011       Ondrej Garncarz
#                  2012       Martin Surkovsky
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
from utils import xml_int, xml_str
import xml.etree.ElementTree as xml
import citems
import undo
import tracing

class Net:

    def __init__(self, project, name, id=None):
        if id is None:
            self.id = project.new_id()
        else:
            self.id = id
        self.project = project
        self.name = name
        self.items = []
        self.change_callback = lambda n: None
        self.change_item_callback = None
        self.undo_manager = undo.UndoManager()

    def get_name(self):
        return self.name

    def get_id(self):
        return self.id

    def is_build_net(self):
        return self.project.build_net == self

    def set_change_callback(self, callback):
        self.change_callback = callback

    def new_id(self):
        return self.project.new_id()

    def add_item(self, item):
        if item.id is None:
            item.id = self.new_id()
        self.items.append(item)
        self.changed()

    def set_name(self, name):
        self.name = name
        self.changed()

    def changed(self):
        self.change_callback(self)

    def changed_item(self, item):
        if self.change_item_callback:
            self.change_item_callback(self, item)

    def add_place(self, position, id = None):
        place = Place(self, id, position)
        self.add_item(place)
        self.changed()
        return place

    def add_transition(self, position, id = None):
        transition = Transition(self, id, position)
        self.add_item(transition)
        self.changed()
        return transition

    def add_edge(self, item1, item2, points, id = None):
        edge = Edge(self, id, item1, item2, points)
        self.add_item(edge)
        self.changed()
        return edge

    def add_area(self, position, size, id = None):
        area = NetArea(self, id, position, size)
        self.add_item(area)
        self.changed()
        return area

    def as_xml(self):
        e = xml.Element("net")
        e.set("name", self.name)
        e.set("id", str(self.id))
        for item in self.items:
            e.append(item.as_xml())
        return e

    def copy(self):
        xml = self.as_xml()
        return load_net(xml, self.project, NewIdLoader(self.project))

    def get_item(self, id):
        for i in self.items:
            if i.get_id() == id:
                return i
        return None

    def places(self):
        return [ item for item in self.items if item.is_place() ]

    def transitions(self):
        return [ item for item in self.items if item.is_transition() ]

    def edges(self):
        return [ item for item in self.items if item.is_edge() ]

    def areas(self):
        return [ item for item in self.items if item.is_area() ]

    def inodes(self):
        return [ item for item in self.items if item.is_inode() ]

    def export_xml(self, build_config):
        e = xml.Element("net")
        e.set("name", self.name)
        e.set("id", str(self.id))

        for place in self.places():
            e.append(place.export_xml(build_config))

        for transition in self.transitions():
            e.append(transition.export_xml(build_config))

        for area in self.areas():
            e.append(area.export_xml())

        return e

    def item_by_id(self, id):
        for item in self.items:
            if item.id == id:
                return item
        return None

    def contains(self, item):
        return item in self.items

    def delete_item(self, item):
        self.items.remove(item)
        self.changed()

    def edges_from(self, item, postprocess=False):
        edges = [ i for i in self.items
                    if i.is_edge() and i.from_item == item ]
        if postprocess:
            edges = [ i.export_form() for i in edges ]
            edges += [ i.make_complement(export_form=True)
                       for i in self.edges_to(item) if i.is_bidirectional() ]
        return edges

    def edges_to(self, item, postprocess=False):
        edges = [ i for i in self.items if i.is_edge() and i.to_item == item ]
        if postprocess:
            edges = [ i.export_form() for i in edges ]
            edges += [ i.make_complement(export_form=True)
                       for i in self.edges_from(item) if i.is_bidirectional() ]
        return edges

    def edges_of(self, item):
        return [ i for i in self.items
                 if i.is_edge() and (i.to_item == item or i.from_item == item) ]

    def edges_out(self):
        return [ i for i in self.items
                 if i.is_edge() and (i.from_item.is_transition() or i.is_bidirectional()) ]

    def trace_nothing(self):
        for i in self.transitions():
            i.trace_fire = False

        for i in self.places():
            i.trace_tokens = False
        self.changed()

    def trace_everything(self):
        for i in self.transitions():
            i.trace_fire = True

        for i in self.places():
            i.trace_tokens = True
        self.changed()


class NetItem(object):

    def __init__(self, net, id):
        self.net = net
        self.id = id

    def get_id(self):
        return self.id

    def changed(self):
        self.net.changed_item(self)

    def is_place(self):
        return False

    def is_transition(self):
        return False

    def is_edge(self):
        return False

    def is_area(self):
        return False

    def is_inode(self):
        return False

    def delete(self):
        self.net.delete_item(self)
        return [ self ]

    def create_xml_element(self, name):
        element =  xml.Element(name)
        element.set("id", str(self.id))
        return element

    def get_canvas_items_dict(self, view_mode):
        result = {}
        for i in self.get_canvas_items(view_mode):
            result[i.kind] = i
        return result

    def get_error_items(self):
        result = []
        messages = self.net.project.get_error_messages(self)
        if not messages:
            return result
        items = self.get_canvas_items_dict(None)
        for name in messages:
            item = items.get(name)
            if item is None:
                # Key was not found, take first item
                # For transition/place it is expected that "box" is returned
                item = self.get_canvas_items()[0]
            position = utils.vector_add(item.get_position(), item.size)
            position = utils.vector_add(position, (0, 0))
            placement = item.get_relative_placement(position)
            error_item = citems.Text(None, "error", placement)
            error_item.delegate_selection = item
            error_item.background_color = (255, 0, 0)
            error_item.border_color = (0, 0, 0)
            error_item.align_y = 0
            error_item.z_level = 20
            error_item.text = messages[name][0]
            result.append(error_item)
        return result


class NetElement(NetItem):

    code = ""

    def __init__(self, net, id, position):
        NetItem.__init__(self, net, id)

        self.box = citems.ElementBox(
            self,
            "box",
            citems.AbsPlacement(position),
            self.default_size,
            self.default_radius)

        self.label_placement = self.box.get_relative_placement(
            utils.vector_add_t(position, self.default_size, 0.5))
        self.label_trace = citems.TraceLabel(self, "tracebox", self.label_placement)
        self.label_trace.text_fn = self.get_trace_label_text

        self.label_simrun = citems.SimRunLabel(self, "simrunbox", self.label_placement)
        self.label_simrun.text_fn = self.get_simrun_label_text

        self.label_verif = citems.VerifLabel(self, "verifbox", self.label_placement)
        self.label_verif.text_fn = self.get_verif_label_text


    def get_canvas_items(self, view_mode):
        items = [ self.box ]
        if view_mode == "tracing":
            items.append(self.label_trace)
        elif view_mode == "verif":
            items.append(self.label_verif)
        elif view_mode == "simrun":
            items.append(self.label_simrun)
        return items

    def has_code(self):
        return self.code.strip() != ""

    def get_code(self):
        return self.code

    def set_code(self, code):
        self.code = code
        self.box.doubleborder = self.has_code()
        self.changed()

    def edges(self):
        return self.net.edges_of(self)

    def edges_from(self, postprocess=False):
        return self.net.edges_from(self, postprocess)

    def edges_to(self, postprocess=False):
        return self.net.edges_to(self, postprocess)

    def delete(self):
        deleted = []
        for edge in self.edges():
            deleted += edge.delete()
        deleted += NetItem.delete(self)
        return deleted

    def xml_code_element(self):
        e = xml.Element("code")
        e.text = self.code
        return e

    def area(self):
        for area in self.net.areas():
            if area.is_inside(self):
                return area

    def get_name(self):
        return self.box.name

    def get_name_or_id(self):
        if not self.box.name:
            return "#{0}".format(self.id)
        return self.box.name

    def set_name(self, name):
        self.box.name = name
        self.changed()


class Transition(NetElement):

    default_size = (70, 36)
    default_radius = 0

    trace_fire = False

    # Collective communication
    collective = False

    # Simrun options
    time_substitution = False
    time_substitution_code = ""
    clock_substitution = False
    clock_substitution_code = ""

    clock = False

    # Verif options
    calls_quit = False
    occurrence_analysis = False
    occurrence_analysis_compare_process = False
    occurrence_analysis_compare_binding = False

    def __init__(self, net, id, position):
        NetElement.__init__(self, net, id, position)
        p = (position[0], position[1] - 20)
        self.guard = citems.Text(self, "guard", self.box.get_relative_placement(p))
        p = (position[0] + self.default_size[0] / 2 + 5, position[1] + 40)
        self.root = citems.Text(self, "root", self.box.get_relative_placement(p))
        self.root.format = "root({0})"

    def get_canvas_items(self, view_mode):
        items = NetElement.get_canvas_items(self, view_mode)
        items.append(self.guard)
        if self.collective:
            items.append(self.root)

        if self.clock:
            p = utils.vector_add(self.box.get_position(), (-9, 7))
            items.append(citems.ClockIcon(
                self, "clock", self.box.get_relative_placement(p)))
        return items

    def set_collective(self, value):
        self.collective = value
        self.box.thicklines = value
        self.changed()

    def is_collective(self):
        return self.collective

    def set_root(self, value):
        self.root.text = value
        self.changed()

    def get_root(self):
        return self.root.text

    def get_time_substitution(self):
        return self.time_substitution

    def set_time_substitution(self, value):
        self.time_substitution = value
        self.changed()

    def get_time_substitution_code(self):
        return self.time_substitution_code

    def set_time_substitution_code(self, value):
        self.time_substitution_code = value
        self.changed()

    def get_clock_substitution(self):
        return self.clock_substitution

    def set_clock_substitution(self, value):
        self.clock_substitution = value
        self.changed()

    def get_clock_substitution_code(self):
        return self.clock_substitution_code

    def set_clock_substitution_code(self, value):
        self.clock_substitution_code = value
        self.changed()

    def set_clock(self, value):
        self.clock = value

    def has_clock(self):
        return self.clock

    def get_priority(self):
        return self.box.corner_text

    def set_priority(self, priority):
        self.box.corner_text = priority
        self.changed()

    def get_priroty(self):
        return self.box.corner_text

    def get_guard(self):
        return self.guard.text

    def set_guard(self, guard):
        self.guard.text = guard
        self.changed()

    def is_transition(self):
        return True

    def is_immediate(self):
        return not self.has_code()

    def as_xml(self):
        e = self.create_xml_element("transition")
        e.set("name", self.box.name)
        e.set("priority", self.get_priority())
        position = self.box.get_position()
        e.set("x", str(position[0]))
        e.set("y", str(position[1]))
        e.set("sx", str(self.box.size[0]))
        e.set("sy", str(self.box.size[1]))
        e.set("clock", str(self.has_clock()))
        e.set("label-x", str(self.label_placement.get_position()[0]))
        e.set("label-y", str(self.label_placement.get_position()[1]))
        e.set("collective", str(self.collective))

        e.append(canvastext_to_xml(self.guard, "guard"))
        if self.has_code():
            e.append(self.xml_code_element())
        if self.trace_fire:
            element = xml.Element("trace")
            element.text = "fire"
            e.append(element)
        if self.time_substitution:
            element = xml.Element("time-substitution")
            element.text = self.time_substitution_code
            e.append(element)

        if self.clock_substitution:
            element = xml.Element("clock-substitution")
            element.text = self.clock_substitution_code
            e.append(element)

        if self.calls_quit:
            element = xml.Element("verif-quit_flag")
            element.text = "True"
            e.append(element)
        if self.occurrence_analysis:
            element = xml.Element("verif-occurrence")
            if self.occurrence_analysis:
                element.set("process", str(self.occurrence_analysis_compare_process))
                element.set("binding", str(self.occurrence_analysis_compare_binding))
            e.append(element)
        if self.collective:
            e.append(canvastext_to_xml(self.root, "root"))
        return e

    def get_trace_label_text(self):
        if self.trace_fire:
            return "fire"
        else:
            return ""

    def get_verif_label_text(self):
        texts = []
        if self.occurrence_analysis:
            texts.append("occurrence")
            if self.occurrence_analysis_compare_process:
                texts[-1] += " +process"
            if self.occurrence_analysis_compare_binding:
                texts[-1] += " +binding"
        if self.calls_quit:
            texts.append("call quit")
        return texts

    def get_simrun_label_text(self):
        items = []
        if self.get_time_substitution():
            items.append("time: {0}".format(self.get_time_substitution_code()))
        if self.get_clock_substitution():
            items.append("clock: {0}".format(self.get_clock_substitution_code()))
        return "\n".join(items)

    def export_xml(self, build_config):
        e = self.create_xml_element("transition")
        e.set("name", self.box.name)
        e.set("guard", self.guard.text)
        e.set("priority", self.get_priority())
        e.set("clock", str(self.has_clock()))
        e.set("collective", str(self.is_collective()))

        if self.has_code():
            e.append(self.xml_code_element())

        if self.is_collective():
            e.set("root", self.get_root())

        for edge in self.edges_to(postprocess=True):
            e.append(edge.create_xml_export_element("edge-in", build_config))

        for edge in self.edges_from(postprocess=True):
            e.append(edge.create_xml_export_element("edge-out", build_config))

        if build_config.tracing:
            # Because of the bug, always trace fire, even it is disabled and self.trace_fire:
            element = xml.Element("trace")
            element.text = "fire"
            e.append(element)

        if build_config.substitutions and self.time_substitution:
            element = xml.Element("time-substitution")
            element.text = self.time_substitution_code
            e.append(element)

        if build_config.substitutions and self.clock_substitution:
            element = xml.Element("clock-substitution")
            element.text = self.clock_substitution_code
            e.append(element)

        if build_config.verification:
            if self.calls_quit:
                element = xml.Element("verif-quit_flag")
                element.text = "True"
                e.append(element)
            if self.occurrence_analysis:
                element = xml.Element("verif-occurrence")
                if self.occurrence_analysis:
                    element.set("process", str(self.occurrence_analysis_compare_process))
                    element.set("binding", str(self.occurrence_analysis_compare_binding))
                e.append(element)
        return e


class Place(NetElement):

    default_size = (0, 0)
    default_radius = 20

    # Verif options
    final_marking = False

    def __init__(self, net, id, position):
        NetElement.__init__(self, net, id, position)

        p = (position[0] + self.box.radius * 0.85, position[1] + self.box.radius * 0.85)
        self.place_type = citems.Text(self, "type", self.box.get_relative_placement(p))

        p = (position[0] + self.box.radius * 0.85, position[1] - self.box.radius * 1.5)
        self.init = citems.Text(self, "init", self.box.get_relative_placement(p))

        p = self.box.get_relative_placement((- self.box.radius - 5, -5), absolute=False)
        self.interface = citems.PlaceInterface(self, "interface", p)

        self.trace_tokens = False
        self.trace_tokens_functions = []

    def get_canvas_items(self, view_mode):
        items = NetElement.get_canvas_items(self, view_mode)
        items.append(self.place_type)
        items.append(self.init)

        if self.interface.is_visible():
            items.append(self.interface)

        return items

    def get_interface_in(self):
        return self.interface.interface_in

    def set_interface_in(self, value):
        self.interface.interface_in = value
        self.interface.update()
        self.changed()

    def get_interface_out(self):
        return self.interface.interface_out

    def set_interface_out(self, value):
        self.interface.interface_out = value
        self.interface.update()
        self.changed()

    def get_radius(self):
        return self.radius

    def get_init_string(self):
        return self.init.text

    def set_place_type(self, place_type):
        self.place_type.text = place_type
        self.changed()

    def set_init_string(self, init_string):
        self.init.text = init_string
        self.changed()

    def get_place_type(self):
        return self.place_type.text

    def is_place(self):
        return True

    def get_trace_label_text(self):
        if self.trace_tokens:
            if not self.trace_tokens_functions:
                return [ "number of tokens" ]
            else:
                return [ trace_function.name for trace_function in self.trace_tokens_functions ]

    def get_verif_label_text(self):
        if self.final_marking:
            return [ "final marking" ]

    def get_simrun_label_text(self):
        return ""

    def tracing_to_xml(self):
        element = xml.Element("trace")
        element.set("trace-tokens", str(self.trace_tokens))
        for trace_function in self.trace_tokens_functions:
            e = xml.Element("function")
            e.set("name", trace_function.name)
            e.set("return-type", trace_function.return_type)
            element.append(e)
        return element

    def interface_as_xml(self):
        element = xml.Element("interface")
        position = self.interface.get_position()
        element.set("x", str(position[0]))
        element.set("y", str(position[1]))
        if self.interface.interface_in is not None:
            element.set("in", self.interface.interface_in)
        if self.interface.interface_out is not None:
            element.set("out", self.interface.interface_out)
        return element

    def as_xml(self):
        e = self.create_xml_element("place")
        position = self.box.get_position()
        e.set("x", str(position[0]))
        e.set("y", str(position[1]))
        e.set("name", str(self.box.name))
        e.set("radius", str(self.box.radius))
        e.set("sx", str(self.box.size[0]))
        e.set("sy", str(self.box.size[1]))
        e.set("label-x", str(self.label_placement.get_position()[0]))
        e.set("label-y", str(self.label_placement.get_position()[1]))

        if self.final_marking:
            element = xml.Element("verif-final-marking")
            element.text = "True"
            e.append(element)

        e.append(canvastext_to_xml(self.place_type, "place-type"))
        e.append(canvastext_to_xml(self.init, "init"))
        if self.has_code():
            e.append(self.xml_code_element())
        if self.interface.is_visible():
            e.append(self.interface_as_xml())
        e.append(self.tracing_to_xml())
        return e

    def export_xml(self, build_config):
        e = self.create_xml_element("place")
        e.set("name", self.box.name)
        e.set("type", self.place_type.text)

        if build_config.verification:
            if self.final_marking:
                element = xml.Element("verif-final-marking")
                element.text = "True"
                e.append(element)

        if not build_config.library or self.interface.interface_in is None:
            e.set("init-expr", self.init.text)
            if self.has_code():
                e.append(self.xml_code_element())

        if build_config.tracing and self.trace_tokens:
            e.append(self.tracing_to_xml())

        if build_config.library:
            if self.interface.interface_in is not None:
                e.set("in", self.interface.interface_in)
            if self.interface.interface_out is not None:
                e.set("out", self.interface.interface_out)
        return e

    def get_final_marking(self):
        return self.final_marking

    def set_final_marking(self, value):
        self.final_marking = value
        self.changed()


class Edge(NetItem):

    size_substitution = False
    size_substitution_code = ""

    def __init__(self, net, id, from_item, to_item, points):
        NetItem.__init__(self, net, id)
        self.from_item = from_item
        self.to_item = to_item
        self.points = [ citems.Point(self, "point", citems.AbsPlacement(p))
                        for p in points ]
        self.line = citems.ArrowLine(self, "line", self.get_all_points)
        self.inscription = citems.Text(self,
                                       "inscription",
                                       self.line.get_relative_placement(None),
                                       "")
        self.label_simrun = citems.SimRunLabel(self, "simrunbox",
                                self.inscription.get_relative_placement((0, 18), absolute=False))
        self.label_simrun.text_fn = self.get_simrun_label_text

    def get_simrun_label_text(self):
        if self.size_substitution:
            return "size: {0}".format(self.size_substitution_code)

    def get_size_substitution(self):
        return self.size_substitution

    def set_size_substitution(self, value):
        self.size_substitution = value
        self.changed()

    def get_size_substitution_code(self):
        return self.size_substitution_code

    def set_size_substitution_code(self, value):
        self.size_substitution_code = value
        self.changed()

    def simple_copy(self):
        """ Copy of edge that preserves topological properties:
            id, inscription, from_item and to_item """
        e = Edge(self.net, self.id, self.from_item, self.to_item, [])
        e.inscription = self.inscription
        e.size_substitution = self.size_substitution
        e.size_substitution_code = self.size_substitution_code
        return e

    def get_canvas_items(self, view_mode):
        items = [ self.line, self.inscription ] + self.points
        if view_mode == "simrun":
            items.append(self.label_simrun)
        return items

    def add_point(self, position):
        inscription_position = self.inscription.get_position()
        for i, (a, b) in enumerate(utils.pairs_generator(self.get_all_points())):
            if utils.is_near_line_segment(a, b, position, 5):
                point = citems.Point(self, "point", citems.AbsPlacement(position))
                point.owner = self
                self.points.insert(i, point)
                break
        self.inscription.set_position(inscription_position)
        self.net.changed() # Canvas items changed, so self.changed() is not sufficient

    def remove_point(self, item):
        inscription_position = self.inscription.get_position()
        self.points.remove(item)
        self.inscription.set_position(inscription_position)
        self.net.changed() # Canvas items changed, so self.changed() is not sufficient

    def get_inscription(self):
        return self.inscription.text

    def set_inscription(self, inscription):
        self.inscription.text = inscription
        self.changed()

    def is_bidirectional(self):
        return self.line.bidirectional

    def toggle_bidirectional(self):
        self.line.bidirectional = not self.line.bidirectional
        self.changed()

    def export_form(self):
        if self.is_bidirectional() and self.from_item.is_place():
            e = self.simple_copy()
            e.size_substitution = False
            return e
        return self

    def make_complement(self, export_form=False):
        """ This function returns exact copy of the edge with changed directions,
            This is used during splitting bidirectional edges """
        e = self.simple_copy()
        e.switch_direction()
        if export_form and e.from_item.is_place():
            e.size_substitution = False

        return e

    def get_end_points(self):
        if self.points:
            p1 = self.points[0].get_position()
            p2 = self.points[-1].get_position()
        else:
            p1 = utils.vector_add_t(
                self.to_item.box.get_position(), self.to_item.box.size, 0.5)
            p2 = utils.vector_add_t(
                self.from_item.box.get_position(), self.from_item.box.size, 0.5)
        return (self.from_item.box.get_border_point(p1),
                self.to_item.box.get_border_point(p2))

    def compute_insciption_point(self):
        points = self.get_all_points()
        if self.inscription_point < len(points) - 1:
            return utils.interpolate(points[self.inscription_point],
                                     points[self.inscription_point + 1],
                                     self.inscription_param)
        else:
            return self.points[self.inscription_point]

    def is_edge(self):
        return True

    def switch_direction(self):
        inscription_position = self.inscription.get_position()
        i = self.from_item
        self.from_item = self.to_item
        self.to_item = i
        self.points.reverse()
        self.inscription.set_position(inscription_position)
        self.changed()

    def as_xml(self):
        e = self.create_xml_element("edge")
        e.set("from_item", str(self.from_item.id))
        e.set("to_item", str(self.to_item.id))
        if self.line.bidirectional:
            e.set("bidirectional", "true")

        e.append(canvastext_to_xml(self.inscription, "inscription"))

        for point in self.points:
            pe = xml.Element("point")
            position = point.get_position()
            pe.set("x", str(position[0]))
            pe.set("y", str(position[1]))
            e.append(pe)

        if self.size_substitution:
            element = xml.Element("size-substitution")
            element.text = self.size_substitution_code
            e.append(element)
        return e

    def get_all_points(self):
        sp, ep = self.get_end_points()
        return [sp] + [ p.get_position() for p in self.points ] + [ep]

    def create_xml_export_element(self, name, build_config):
        e = xml.Element(name)
        e.set("id", str(self.id))
        if self.from_item.is_place():
            e.set("place-id", str(self.from_item.get_id()))
        else:
            e.set("place-id", str(self.to_item.get_id()))
        e.set("expr", self.inscription.text)

        if build_config.substitutions and self.size_substitution:
            element = xml.Element("size-substitution")
            element.text = self.size_substitution_code
            e.append(element)

        return e


class RectItem(NetItem):

    def __init__(self, net, id, position, size):
        NetItem.__init__(self, net, id)
        self.point1 = citems.Point(self, "point1", citems.AbsPlacement(position))
        self.point1.action = "resize_ltop"
        self.point2 = citems.Point(self, "point2", citems.AbsPlacement(
            utils.vector_add(position, size)))
        self.point2.owner = self
        self.point2.action = "resize_rbottom"

    def get_size(self):
        return utils.make_vector(self.point1.get_position(), self.point2.get_position())

    def is_inside(self, item):
        return utils.position_inside_rect(
            item.box.get_position(),
            self.point1.get_position(),
            self.get_size())


class NetArea(RectItem):

    def __init__(self, net, id, position, size):
        RectItem.__init__(self, net, id, position, size)
        self.area = citems.Area(self, "area", self.point1, self.point2)

        position = utils.vector_add(self.point1.get_position(), (0, -15))
        self.init = citems.Text(self, "init", self.point1.get_relative_placement(position))

    def get_canvas_items(self, view_mode):
        return [ self.point1, self.point2,
                 self.init, self.area ]

    def set_init_expr(self, init_expr):
        self.init.text = init_expr
        self.changed()

    def get_init_expr(self):
        return self.init.text

    def is_area(self):
        return True

    def as_xml(self):
        e = self.create_xml_element("area")
        position = self.point1.get_position()
        size = self.get_size()
        e.set("x", str(position[0]))
        e.set("y", str(position[1]))
        e.set("sx", str(size[0]))
        e.set("sy", str(size[1]))
        e.append(canvastext_to_xml(self.init, "init"))
        return e

    def export_xml(self):
        e = self.create_xml_element("area")
        e.set("init-expr", self.init.text)
        e.set("name", "")
        items = [ item for item in self.net.places() if self.is_inside(item) ]
        for item in items:
            element = xml.Element("place")
            element.set("id", str(item.id))
            e.append(element)
        return e

    def places(self):
        return [ place for place in self.net.places() if self.is_inside(place) ]

    def transitions(self):
        return [ transition for transition in self.net.transitions()
                 if self.is_inside(transition) ]


class BasicLoader:
    """
        Loads an element id from xml and preserves original id
    """
    def __init__(self, project):
        self.project = project

    def get_id(self, element):
        id = utils.xml_int(element, "id", 0)
        self.project.id_counter = max(self.project.id_counter, id)
        return id

    def translate_id(self, id):
        return id


class NewIdLoader:
    """
        Loads an element id from xml and assign new id
    """

    def __init__(self, project):
        self.project = project
        self.idtable = {}

    def get_id(self, element):
        id = utils.xml_int(element, "id", 0)
        new_id = self.project.new_id()
        self.idtable[id] = new_id
        return new_id

    def translate_id(self, id):
        return self.idtable[id]


def canvastext_to_xml(obj, name):
    element = xml.Element(name)
    position = obj.get_position()
    element.set("x", str(position[0]))
    element.set("y", str(position[1]))
    element.text = obj.text
    return element

def canvastext_from_xml(element, obj):
    position = (utils.xml_int(element, "x"),
                utils.xml_int(element, "y"))
    obj.set_position(position)
    if element.text is None:
        obj.text = ""
    else:
        obj.text = element.text

def load_code(element):
    if element.find("code") is not None:
        return element.find("code").text
    else:
        return ""

def load_place_tracing(element, place):
    if element is None:
        return
    place.trace_tokens = utils.xml_bool(element, "trace-tokens", False)
    for e in element.findall("function"):
        name = e.get("name")
        return_type = e.get("return-type")
        place.trace_tokens_functions.append(tracing.TraceFunction(name, return_type))

def load_place(element, net, loader):
    id = loader.get_id(element)
    place = net.add_place((xml_int(element,"x"), xml_int(element, "y")), id)
    place.box.name = xml_str(element, "name", "")
    place.box.radius = xml_int(element,"radius")
    place.box.size = (xml_int(element,"sx", 0), xml_int(element,"sy", 0))

    if element.get("label-x") and element.get("label-y"):
        label_x = xml_int(element, "label-x")
        label_y = xml_int(element, "label-y")
        place.label_placement.set_position((label_x, label_y))

    if element.find("place-type") is not None:
        canvastext_from_xml(element.find("place-type"), place.place_type)
    else:
        place.place_type.text = element.get("place_type", "") # Backward compatability

    if element.find("init") is not None:
        canvastext_from_xml(element.find("init"), place.init)
    else:
        place.init.text = element.get("init_string", "") # Backward compatability

    place.set_code(load_code(element))

    load_place_tracing(element.find("trace"), place)

    interface = element.find("interface")
    if interface is not None:
        place.interface.set_position((xml_int(interface, "x"),
                                      xml_int(interface, "y")))
        place.interface.interface_in = interface.get("in")
        place.interface.interface_out = interface.get("out")
        place.interface.update()

    e = element.find("verif-final-marking")
    if e is not None:
        place.final_marking = bool(e.text)

def load_transition(element, net, loader):
    id = loader.get_id(element)
    transition = net.add_transition((xml_int(element,"x"), xml_int(element, "y")), id)
    sx = xml_int(element,"sx")
    sy = xml_int(element,"sy")

    if element.get("label-x") and element.get("label-y"):
        label_x = xml_int(element, "label-x")
        label_y = xml_int(element, "label-y")
        transition.label_placement.set_position((label_x, label_y))

    transition.box.size = (sx, sy)
    transition.box.name = xml_str(element,"name", "")
    if element.find("guard") is not None:
        canvastext_from_xml(element.find("guard"), transition.guard)
    else:
        transition.guard.text = element.get("guard", "") # Backward compatability

    if element.find("root") is not None:
        canvastext_from_xml(element.find("root"), transition.root)

    transition.set_code(load_code(element))
    transition.trace_fire = element.find("trace") is not None
    transition.clock = utils.xml_bool(element, "clock", False)
    transition.set_collective(utils.xml_bool(element, "collective", False))

    if element.find("time-substitution") is not None:
        transition.time_substitution = True
        transition.time_substitution_code = element.find("time-substitution").text
    if element.find("clock-substitution") is not None:
        transition.clock_substitution = True
        transition.clock_substitution_code = element.find("clock-substitution").text

    e = element.find("verif-occurrence")
    if e is not None:
        transition.occurrence_analysis = True
        transition.occurrence_analysis_compare_process = utils.xml_bool(e, "process")
        transition.occurrence_analysis_compare_binding = utils.xml_bool(e, "binding")

    transition.set_priority(element.get("priority", ""))

def load_edge(element, net, loader):
    id = loader.get_id(element)
    fitem = net.item_by_id(loader.translate_id(xml_int(element, "from_item")))
    assert fitem is not None
    titem = net.item_by_id(loader.translate_id(xml_int(element, "to_item")))
    assert titem is not None
    points = [ (xml_int(e, "x"), xml_int(e,"y")) for e in element.findall("point") ]
    edge = net.add_edge(fitem, titem, points, id)
    edge.line.bidirectional = utils.xml_bool(element, "bidirectional", False)

    if element.find("inscription") is not None:
        canvastext_from_xml(element.find("inscription"), edge.inscription)
    else: # Backward compitabality
        if element.get("inscription") is not None:
            edge.inscription.text = xml_str(element, "inscription")

    if element.find("size-substitution") is not None:
        edge.size_substitution = True
        edge.size_substitution_code = element.find("size-substitution").text


def load_area(element, net, loader):
    id = loader.get_id(element)
    sx = xml_int(element,"sx")
    sy = xml_int(element,"sy")
    px = xml_int(element, "x")
    py = xml_int(element, "y")
    area = net.add_area((px, py), (sx, sy), id)
    if element.find("init") is not None:
        canvastext_from_xml(element.find("init"), area.init)
    else:
        area.init.text = xml_str(element,"init-expr", "")

def load_net(element, project, loader):
    name = element.get("name", "Main") # Setting "Main" for backward compatability
    id = loader.get_id(element)

    net = Net(project, name, id)

    for e in element.findall("area"):
        load_area(e, net, loader)

    for e in element.findall("place"):
        load_place(e, net, loader)

    for e in element.findall("transition"):
        load_transition(e, net, loader)

    for e in element.findall("edge"):
        load_edge(e, net, loader)

    return net
