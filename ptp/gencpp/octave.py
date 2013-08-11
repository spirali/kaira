#
#    Copyright (C) 2012, 2013 Stanislav Bohm
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

from base.writer import Writer

class OctaveBuilder(Writer):

    def __init__(builder, project):
        Writer.__init__(builder)
        builder.project = project


def write_loader(builder, oct_file):
    builder.line("autoload(\"{0}_init\", file_in_loadpath (\"{1}\"))",
              builder.project.get_name(),
              oct_file)

    for net in builder.project.nets:
        builder.line("autoload(\"{0}\", file_in_loadpath (\"{1}\"))", net.name, oct_file)
    builder.line("{0}_init()", builder.project.get_name())

def write_oct_file(builder):
    builder.line("#include <caoctave.h>")
    builder.line("#include \"{0}.h\"", builder.project.get_name())
    builder.emptyline()

    for net in builder.project.nets:
        write_oct_function(builder, net)

    # init function
    builder.emptyline()
    builder.line("DEFUN_DLD({0}_init, args, , \"Library init\")", builder.project.get_name())
    builder.block_begin()
    builder.line("calib_init(0, NULL);")
    builder.line("return octave_value(\"Library '{0}' is ready\");", builder.project.get_name())
    builder.block_end()

def write_oct_function(builder, net):
    input_places = net.get_input_places()
    output_places = net.get_output_places()
    output_places_only = [ place for place in output_places
                                 if place not in input_places ]
    inputs = [ place.interface_input for place in input_places ]
    outputs = [ place.interface_output for place in output_places ]
    outputs_only = [ place.interface_output for place in output_places_only ]

    description = "[{2}]={0}({1}) ({3})->({4})".format(
            net.name,
            ",".join(inputs),
            ",".join(outputs),
            ",".join(place.type for place in input_places),
            ",".join(place.type for place in input_places))

    builder.line("DEFUN_DLD({0}, $args, , \"{1}\")", net.name, description)
    builder.block_begin()

    for i, place in enumerate(input_places):
        builder.line("{0.type} {0.interface_input};", place)
        builder.line("caoctave::from_octave_value({0.interface_input}, $args({1}));", place, i)

    for place in output_places_only:
        builder.line("{0.type} {0.interface_output};", place, i)

    builder.line("{0}({1});", net.name, ",".join(inputs + outputs_only))
    builder.line("octave_value_list $result({0});", len(outputs))
    for i, name in enumerate(outputs):
        builder.line("$result({0}) = caoctave::to_octave_value({1});", i, name)
    builder.line("return $result;")

    builder.block_end()
    builder.emptyline()
