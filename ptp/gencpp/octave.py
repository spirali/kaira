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

from base.neltypes import t_int, t_string, t_float, t_double, t_bool
from base.writer import Writer
import library

class OctaveBuilder(Writer):

    def __init__(builder, project):
        Writer.__init__(builder)
        builder.project = project


def write_loader(builder, oct_file):
    builder.line("autoload(\"{0}_init\", file_in_loadpath (\"{1}\"))",
              builder.project.get_name(),
              oct_file)

    for net in builder.project.get_modules():
        builder.line("autoload(\"{0}\", file_in_loadpath (\"{1}\"))", net.name, oct_file)
    builder.line("{0}_init()", builder.project.get_name())

def write_oct_conversions(builder):
    builder.line("std::vector<std::vector<double > > Matrix_to_double_vectors(const Matrix &matrix)")
    builder.block_begin()
    builder.line("std::vector<std::vector<double > > result;")
    builder.line("for(int i=0;i<matrix.rows();i++)")
    builder.block_begin()
    builder.line("std::vector<double> row;")
    builder.line("for(int j=0;j<matrix.columns();j++)")
    builder.block_begin()
    builder.line("row.push_back(matrix(i,j));")
    builder.block_end()
    builder.line("result.push_back(row);")
    builder.block_end()
    builder.line("return result;")
    builder.block_end()

    builder.emptyline()

    builder.line("Matrix Double_vectors_to_Matrix(const std::vector<std::vector<double > > &data)")
    builder.block_begin()
    builder.line("int rows=data.size(), columns=0;")
    builder.line("for(int i=0;i<data.size();i++)")
    builder.block_begin()
    builder.line("if (data[i].size()>columns) columns = data[i].size();")
    builder.block_end()
    builder.line("Matrix result(rows,columns);")
    builder.line("for(int i=0;i<data.size();i++)")
    builder.block_begin()
    builder.line("std::vector<double> row=data[i];")
    builder.line("for(int j=0;j<row.size();j++)")
    builder.block_begin()
    builder.line("result(i,j)=row[j];")
    builder.block_end()
    builder.block_end()
    builder.line("return result;")
    builder.block_end()

    builder.emptyline()

    builder.raw_text("""
    Matrix Array_Double_to_oct_Matrix(const std::vector<double> &v)
    {
        Matrix matrix(v.size(), 1);
        for(int i=0; i<v.size(); i++) {
            matrix(i, 0) = v[i];
        }
        return matrix;
    }

    std::vector<double> oct_Matrix_to_Array_Double(const Matrix &matrix)
    {
        std::vector<double> v;
        for(int i=0; i<matrix.rows(); i++) {
            v.push_back(matrix(i, 0));
        }
        return v;
    }
    """)

def write_oct_file(builder, header_filename):
    builder.line("#include <octave/oct.h>")
    builder.line("#include \"{0}\"", header_filename)
    builder.emptyline()
    write_oct_conversions(builder, )
    builder.emptyline()
    for etype in builder.project.get_extern_types():
        if etype.is_octave_value():
            source = ("*{0}/{1}".format(etype.get_name(), "from_octave_value"), 1)
            builder.write_function(
                "{0.rawtype} {0.name}_from_octave_value(const octave_value &obj)".format(etype),
                etype.get_code("from_octave_value"),
                source)
            builder.emptyline()
            source = ("*{0}/{1}".format(etype.get_name(), "to_octave_value"), 1)
            builder.write_function(
                "octave_value {0.name}_to_octave_value(const {0.rawtype} &obj)".format(etype),
                etype.get_code("to_octave_value"),
                source)
            builder.emptyline()

    for net in builder.project.get_modules():
        write_oct_function(builder, net)

    # init function
    builder.emptyline()
    builder.line("DEFUN_DLD({0}_init, args, , \"Library init\")", builder.project.get_name())
    builder.block_begin()
    builder.line("calib_init(0, NULL);")
    builder.line("return octave_value(\"Library is ready\");")
    builder.block_end()

def write_oct_function(builder, net):
    context = net.get_interface_context()

    input_vars = library.get_library_input_arguments(net)
    input_description = ",".join(input_vars)

    output_vars = library.get_library_output_arguments(net)
    output_description = ",".join(output_vars)

    decl = library.emit_library_function_declaration(builder.emitter, net)
    fn_description = "void {0}({1})".format(net.name, decl)

    description = "Function: {0}, usage ({1})->({2})".format(fn_description,
                                                             input_description,
                                                             output_description)

    builder.line("DEFUN_DLD({0}, args, , \"{1}\")", net.get_name(), description)
    builder.block_begin()
    builder.line("octave_value_list result;")

    for i, input_variable in enumerate(input_vars):
        input_type = context[input_variable]
        input_reference="args({0})".format(i)
        builder.line("{0} _{1} = {2};",
                     builder.emitter.emit_type(input_type),
                     input_variable,
                     emit_parameter_form_octave_value(builder.project, input_reference, input_type))

    for only_output_variable in library.get_library_output_only_arguments(net):
        builder.line("{0} _{1};",
                  builder.emitter.emit_type(context[only_output_variable]),
                  only_output_variable)

    function_arguments = library.get_library_function_arguments(net)
    builder.line("{0}({1});",net.name,",".join("_"+x for (x,y) in function_arguments))

    for i, output_variable in enumerate(output_vars):
        builder.line("result({0}) = {1};", i,
                     emit_result_to_octave_value(builder.project, "_" + output_variable,
                                                 context[output_variable]))

    builder.line("return result;")
    builder.block_end()
    builder.emptyline()

def emit_parameter_form_octave_value(project, parameter, t):
    if isinstance(t, str):
        raise Exception("'{0}' cannot be converted from octave value".format(t))

    if t.name == "":
        return t.get_safe_name()
    a = t.get_arity()
    if a == 0:
        if t.name == "Int":
            return parameter+".int_value()"
        elif t.name == "String":
            return parameter+".string_value()"
        elif t.name == "Bool":
            return parameter+".bool_value()"
        elif t.name == "Double":
            return parameter+".double_value()"
        elif t.name == "Float":
            return parameter+".float_value()"
        etype = project.get_extern_type(t.name)
        if etype and etype.is_octave_value():
            return "{0}_from_octave_value({1})".format(etype.name,parameter)

    if a == 1 and t.name == "Array":
        inner_type = t.args[0]
        if inner_type.name == "Double":
            return "oct_Matrix_to_Array_Double({0}.matrix_value())".format(parameter);
        if inner_type.get_arity() == 1 and inner_type.name == "Array":
            inner_type = inner_type.args[0]
            if inner_type.name == "Double":
                return "Matrix_to_double_vectors({0}.matrix_value())".format(parameter)

    raise Exception("Type '{0}' cannot be converted from octave value".format(t))

def emit_result_to_octave_value(project, result, t):
    if isinstance(t, str):
        raise Exception("'{0}' cannot be converted to octave value".format(t))
    if t.name == "":
        return t.get_safe_name()
    if t in [ t_int, t_string, t_bool, t_float, t_double ]:
        return "octave_value({0})".format(result)
    etype = project.get_extern_type(t.name)
    if etype and etype.is_octave_value():
            return "{0}_to_octave_value({1})".format(etype.name,result)
    if  t.get_arity() == 1 and t.name == "Array":
        inner_type = t.args[0]
        if inner_type.name == "Double":
            return "Array_Double_to_oct_Matrix({0})".format(result);
        if inner_type.get_arity() == 1 and inner_type.name == "Array":
            inner_type = inner_type.args[0]
            if inner_type == t_double:
                return "Double_vectors_to_Matrix({0})".format(result)

    raise Exception("Type '{0}' cannot be converted to octave value".format(t))
