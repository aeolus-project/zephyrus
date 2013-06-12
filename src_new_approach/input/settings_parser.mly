/****************************************************************************/
/*                                                                          */
/*    This file is part of Zephyrus.                                        */
/*                                                                          */
/*    Zephyrus is free software: you can redistribute it and/or modify      */
/*    it under the terms of the GNU General Public License as published by  */
/*    the Free Software Foundation, either version 3 of the License, or     */
/*    (at your option) any later version.                                   */
/*                                                                          */
/*    Zephyrus is distributed in the hope that it will be useful,           */
/*    but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         */
/*    GNU General Public License for more details.                          */
/*                                                                          */
/*    You should have received a copy of the GNU General Public License     */
/*    along with Zephyrus.  If not, see <http://www.gnu.org/licenses/>.     */
/*                                                                          */
/****************************************************************************/

/* Depends on
    - input/Settings (obviously)
*/

%token Input_file_universe Input_file_repositories Input_file_initial_configuration Input_file_specification Input_optimization_function
%token Verbose_warning_setting_not_set
%token Equals Left_bracket Right_bracket Left_paren Right_paren Comma Semicolon EOF
%token<bool> Bool
%token<string> Ident


%start main
%type<unit> main


%%

main:
  | EOF   { () }
  | element main { () }

element:
  | element_input   { () }
  | element_verbose { () }

element_input:
  | Input_file_universe              Equals Ident { Settings.input_file_universe              := Some($3) }
  | Input_file_repositories          Equals Left_bracket repository_list Right_bracket { () }
  | Input_file_initial_configuration Equals Ident { Settings.input_file_initial_configuration := Some($3) }
  | Input_file_specification         Equals Ident { Settings.input_file_specification         := Some($3) }
  | Input_optimization_function      Equals Ident { Settings.input_optimization_function      := Some(Settings.optimization_function_of_string $3) }

repository_list:
  |  { () }
  | Left_paren Ident Comma Ident Right_paren { Settings.input_file_repositories := ($2, $4)::!Settings.input_file_repositories }
  | Left_paren Ident Comma Ident Right_paren Semicolon repository_list { Settings.input_file_repositories := ($2, $4)::!Settings.input_file_repositories }

element_verbose:
  | Verbose_warning_setting_not_set Equals Bool { Settings.verbose_warning_setting_not_set := $3 }

