#
#    Copyright (C) 2013 Stanislav Bohm,
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

import gtk
import gtkutils
import mainwindow
from canvas import Canvas
import utils
import cconfig
import citems


class NetViewCanvasConfig(cconfig.NetCanvasConfig):

    selection = False

    def __init__(self, view):
        cconfig.NetCanvasConfig.__init__(self)
        self.perspective = None
        self.view = view
        self.token_boxes = {} # Canvas items for tokens by place.id
        self.activations = {} # Canvas items for activations by transition.id
        self.packet_boxes = {} # Canvas items for packet by edge.id

    def set_perspective(self, perspective):
        self.perspective = perspective
        self.set_net(self.perspective.runinstance.net)
        self.configure()

    def configure_item(self, item):
        cconfig.NetCanvasConfig.configure_item(self, item)

    def collect_items(self):
        items = cconfig.NetCanvasConfig.collect_items(self)
        for item in items:
            item.action = None
        if self.net is not None:
            items += self.get_token_items()
            items += self.get_activation_items()
            items += self.get_packet_items()
        return items

    def get_activation_items(self):
        result = []
        for transition in self.perspective.runinstance.net.transitions():
            activations = self.activations.get(transition.id)
            if activations is None:
                position = utils.vector_add(
                    transition.box.get_position(), (0, transition.box.size[1] + 10))
                activations = citems.TransitionActivations(
                    None, "activations", citems.AbsPlacement(position))
                self.activations[transition.id] = activations
            values = self.perspective.get_activations_values(transition)
            if values:
                result.append(activations)
                result += activations.create_activations(values)
        return result

    def get_token_items(self):
        places = self.perspective.runinstance.net.places()
        result = []
        for place in places:
            token_box = self.token_boxes.get(place.id)
            if token_box is None:
                sx, sy = place.box.size
                position = utils.vector_add(place.box.get_position(),
                                            (sx + 20, sy / 2))
                token_box = citems.TokenBox(None, "tokenbox", citems.AbsPlacement(position))
                self.token_boxes[place.id] = token_box
            token_box.set_tokens(self.perspective.get_tokens(place),
                                 self.perspective.get_new_tokens(place),
                                 self.perspective.get_removed_tokens(place))
            result.append(token_box)
        return result

    def get_packet_items(self):
        def get_size(self, cr):
            if not self.texts:
                return
            tx = max(utils.text_size(cr, t)[0] for t in self.texts)
            tx += 20
            ty = 13 * len(self.texts) + 4
            return (tx, ty)

        result = []
        color = (0.8, 0.3, 0.1, 0.85)
        color_active = (1, 1, 1)
        color_inactive = (0.8, 0.8, 0.8)
        for edge in self.perspective.runinstance.net.edges_out():
            packets = self.perspective.get_packets_info(edge.id)
            packet_box = self.packet_boxes.get(edge.id)
            if packet_box is None:
                position = utils.vector_add(edge.inscription.get_position(),
                                            (0, 15))
                placement = citems.AbsPlacement(position)
                packet_box = citems.Box(None,
                                        "packetbox",
                                        placement)
                packet_box.size_fn = get_size
                packet_box.background = color
                packet_box.radius = 5

                self.packet_boxes[edge.id] = packet_box
            result.append(packet_box)
            packet_box.texts = [ p[3] for p in packets ]
            for i, (process_id, origin_id, top, text) in enumerate(packets):
                position = utils.vector_add(packet_box.get_position(),
                                            (10, 13 * i))
                t = citems.Text(
                    None,
                    "packet",
                    packet_box.get_relative_placement(position),
                    text)
                if top:
                    t.color = color_active
                    t.packet_data = (process_id, origin_id)
                else:
                    t.color = color_inactive
                    t.packet_data = None
                t.padding_y = 4
                t.z_level = 15
                t.action = None
                result.append(t)
        return result

    def on_item_click(self, item, position):
        if item.kind == "box":
            if item.owner.is_place():
                self.view.open_tokens_tab(item.owner)
            if item.owner.is_transition():
                self.view.open_transition_tab(item.owner)


class NetView(gtk.HPaned):

    def __init__(self, app, config=None, other_tabs=None, other_widgets=None):
        gtk.HPaned.__init__(self)
        self.app = app
        if other_tabs:
            notebook = gtk.Notebook()
            notebook.append_page(self._perspectives(other_widgets), gtk.Label("Views"))
            for name, widget in other_tabs:
                notebook.append_page(widget, gtk.Label(name))
            self.perspectives.hide_headers()
            self.pack1(notebook)
        else:
            self.pack1(self._perspectives(other_widgets), False)
        self.config = config
        self.canvas = Canvas(self.config, zoom=1)
        self.pack2(self.canvas, True)
        self.show_all()

    def set_config(self, config):
        self.config = config
        self.canvas.set_config(config)

    def redraw(self):
        self.canvas.redraw()

    def save_as_svg(self, filename):
        self.canvas.save_as_svg(filename)

    def get_perspective(self):
        return self.perspectives.get_selection(0)

    def set_runinstance(self, runinstance):
        self.runinstance = runinstance
        self._refresh_perspectives(runinstance.get_perspectives())

    def _refresh_perspectives(self, perspectives):
        p = self.get_perspective()
        if p is not None:
            name = self.get_perspective().name
        else:
            name = None
        selected = False
        self.perspectives.clear()
        for p in perspectives:
            i = self.perspectives.append((p, str(p.name)))
            if name == p.name:
                self.perspectives.select_iter(i)
                selected = True
        if not selected:
            self.perspectives.select_first()
        self._perspectives_changed(None)

    def _perspectives(self, other_widgets):
        self.perspectives = gtkutils.SimpleList((("_", object), ("Views",str)))
        self.perspectives.set_size_request(80,10)
        self.perspectives.connect_view("cursor-changed", self._perspectives_changed);

        if other_widgets:
            box = gtk.VBox()
            box.pack_start(self.perspectives, True, True)
            for widget in other_widgets:
                box.pack_start(widget, False, False)
            return box
        else:
            return self.perspectives

    def _perspectives_changed(self, w):
        perspective = self.get_perspective()
        if perspective is not None:
            self.config.set_perspective(perspective)
        else:
            self.config.set_perspective(None)
        self.redraw()

    def open_tokens_tab(self, place):
        text_buffer = gtk.TextBuffer()
        t = self.get_perspective().get_removed_tokens(place)
        removed_tokens = "\n".join("consumed: " + token for token in map(str, t))

        tokens = self.get_perspective().get_tokens(place)
        tokens = utils.collapse_line_repetitions(tokens)
        tokens = "\n".join(tokens)

        new_tokens = "\n".join(
                utils.collapse_line_repetitions(self.get_perspective().get_new_tokens(place)))

        if removed_tokens != "" and tokens != "":
            removed_tokens = removed_tokens + "\n"
        if tokens != "" and new_tokens != "":
            tokens = tokens + "\n"

        tag_removed = text_buffer.create_tag('removed', font="Italic")
        tag_new = text_buffer.create_tag('new', font="Bold")
        text_buffer.insert_with_tags(text_buffer.get_end_iter(), removed_tokens, tag_removed)
        text_buffer.insert(text_buffer.get_end_iter(), tokens)
        text_buffer.insert_with_tags(text_buffer.get_end_iter(), new_tokens, tag_new)
        text_area = gtk.TextView()
        text_area.set_buffer(text_buffer)
        text_area.set_editable(False)

        sw = gtk.ScrolledWindow()
        sw.add(text_area)
        sw.show_all()

        label = "Tokens of " + place.get_name()
        self.app.window.add_tab(mainwindow.Tab(label, sw))

    def open_transition_tab(self, transition):
        values = self.get_perspective().get_transition_trace_values(transition)
        if values:
            text_buffer = gtk.TextBuffer()
            text_buffer.insert(text_buffer.get_end_iter(), "\n".join(map(str, values)))
            text_area = gtk.TextView()
            text_area.set_buffer(text_buffer)
            text_area.set_editable(False)

            sw = gtk.ScrolledWindow()
            sw.add(text_area)
            sw.show_all()

            if transition.get_name() == "":
                label = "Traced values in T" + str(transition.get_id())
            else:
                label = "Traced values in " + transition.get_name()
            self.app.window.add_tab(mainwindow.Tab(label, sw))
