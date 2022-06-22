

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"
#include <map>
#include <queue>

extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any 
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}



ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {
    install_basic_classes();
}

void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
	class_(Object, 
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);

    basic_classes.push_back(Object_class);

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
	class_(IO, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       single_Features(method(out_string, single_Formals(formal(arg, Str)),
										      SELF_TYPE, no_expr())),
							       single_Features(method(out_int, single_Formals(formal(arg, Int)),
										      SELF_TYPE, no_expr()))),
					       single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
			       single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	       filename);  

    basic_classes.push_back(IO_class);

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
	class_(Int, 
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);

    basic_classes.push_back(Int_class);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

    basic_classes.push_back(Bool_class);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    Class_ Str_class =
	class_(Str, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       append_Features(
									       single_Features(attr(val, Int, no_expr())),
									       single_Features(attr(str_field, prim_slot, no_expr()))),
							       single_Features(method(length, nil_Formals(), Int, no_expr()))),
					       single_Features(method(concat, 
								      single_Formals(formal(arg, Str)),
								      Str, 
								      no_expr()))),
			       single_Features(method(substr, 
						      append_Formals(single_Formals(formal(arg, Int)), 
								     single_Formals(formal(arg2, Int))),
						      Str, 
						      no_expr()))),
	       filename);

    basic_classes.push_back(Str_class);
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
} 



/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    ClassTable *classtable = new ClassTable(classes);

    /* some semantic analysis code may go here */
    // assignment PA4

    if (classtable->check_inheritance(classes)) {
        Environment *environment = classtable->init_methods_info(classes);
        ((SymbolTableEnvironment *)environment)->print_elements();
    }

    if (classtable->errors()) {
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
    }
}

bool ClassTable::check_inheritance(Classes classes) {
    std::vector<Class_> all_classes(basic_classes); //copy
    for (int i = classes->first(); classes->more(i); i = classes->next(i))
        all_classes.push_back(classes->nth(i));
    InheritanceChecker *ic = new GraphInheritanceChecker();
    bool result = true;
    if (!ic->check(all_classes)) {
        result = false;
        semant_errors++;
        cerr << "Inheritance Error!~~" << endl;
    }
    delete ic;
    std::vector<Class_>().swap(all_classes);
    return result;
}

bool GraphInheritanceChecker::check(std::vector<Class_> all_classes) {
    int cnt = 0;
    std::map<Symbol, int> class2id;
    for (int i = 0; i < all_classes.size(); i++) {
        Class_ class_ = all_classes[i];
        // mapping from Class_ to id
        if (class2id.find(class_->get_name()) == class2id.end()) 
           class2id[class_->get_name()] = cnt++;
    }
    int num_vertices = cnt;
    std::vector<int> *graph = new std::vector<int>[num_vertices];
    int *indegree = new int[num_vertices];
    memset(indegree, 0, sizeof(int)*num_vertices);
    // Insert edges
    for (int i = 0; i < all_classes.size(); i++) {
        Class_ class_ = all_classes[i];
        int class_id = class2id[class_->get_name()];
        int parent_id = class2id[class_->get_parent()];
        if (class_id != parent_id) {
            graph[class_id].push_back(parent_id);
            indegree[parent_id]++;
        }
    }
    // top sort
    std::queue<int> qq;
    cnt = 0;
    for (int v = 0; v < num_vertices; v++) {
        if (indegree[v] == 0) {
            qq.push(v);
            cnt++;
        }
    }
    while (!qq.empty()) {
        int v = qq.front();
        qq.pop();
        for (int i = 0; i < graph[v].size(); i++) {
            int w = graph[v][i];
            if (--indegree[w] == 0) {
                qq.push(w);
                cnt++;
            }
        }
    }
    std::map<Symbol, int>().swap(class2id);
    for (int v = 0; v < num_vertices; v++)
        std::vector<int>().swap(graph[v]);
    delete indegree;
    return cnt == num_vertices;
}

Symbol class__class::get_name() {
    return name;
}

Symbol class__class::get_parent() {
    return parent;
}

Environment *ClassTable::init_methods_info(Classes classes) {
    Environment *environment = new SymbolTableEnvironment();
    for (int i = classes->first(); classes->more(i); i = classes->next(i))
        classes->nth(i)->init_methods_info(environment);
    return environment;
}

void class__class::init_methods_info(Environment *environment) {
    method_class *method;
    for (int i = features->first(); features->more(i); i = features->next(i))
        if (features->nth(i)->is_method()) {
            method = (method_class *)features->nth(i);
            method->init_info(environment, name);
        }
}

void method_class::init_info(Environment *environment, Symbol class_name) {
    PMethodTypes method_types = new struct MethodTypes();
    for (int i = formals->first(); formals->more(i); i = formals->next(i))
        formals->nth(i)->add_type(method_types->formal_types);
    method_types->return_type = return_type;
    environment->push_method(class_name, name, method_types);
}

void formal_class::add_type(std::vector<Symbol>& types) {
    types.push_back(type_decl);
}

bool method_class::is_attribute() { return false; }
bool method_class::is_method() { return true; }
bool attr_class::is_attribute() { return true; }
bool attr_class::is_method() { return false; }



//Symbol Table
//

SymbolTableEnvironment::SymbolTableEnvironment() {}
SymbolTableEnvironment::~SymbolTableEnvironment() {
    for (int i = 0; i < elements.size(); i++)
        delete elements[i];
    std::vector<PElem>().swap(elements);
}

int SymbolTableEnvironment::push_var(Symbol name, Symbol type)  {
    elements.push_back(new VarElem(name, type));
    return elements.size() - 1;
}

Symbol SymbolTableEnvironment::find_var_type(Symbol name) {
    PElem elem;
    for (int i = elements.size() - 1; i >= 0; i--) {
        elem = elements[i];
        if (elem->is_var() && ((VarElem *)elem)->get_name() == name) // ? strcmp  elem->get_name()->get_string(), name->get_string()
            return ((VarElem *)elem)->get_type();
    }
    return NULL; 
}

int SymbolTableEnvironment::push_method(Symbol class_name, Symbol method_name, PMethodTypes method_types) {
    elements.push_back(new MethodElem(class_name, method_name, method_types));
    return elements.size() - 1;
}

PMethodTypes SymbolTableEnvironment::find_method_type(Symbol class_name, Symbol method_name) {
    PElem elem;
    for (int i = elements.size() - 1; i >= 0; i--) {
        elem = elements[i];
        if (elem->is_method() 
            && ((MethodElem *)elem)->get_class_name() == class_name 
            && ((MethodElem *)elem)->get_method_name() == method_name)
            
          return ((MethodElem *)elem)->get_method_types();
    }
    return NULL;
}

int SymbolTableEnvironment::pop() {
    PElem elem = elements[elements.size() - 1];
    delete elem;
    elements.pop_back();
    return elements.size() - 1;
}

void SymbolTableEnvironment::print_elements() {
    PElem elem;
    for (int i = 0; i < elements.size(); i++) {
        elem = elements[i];
        if (elem->is_var()) {
            printf("VAR <NAME:%s, TYPE:%s>\n", 
                ((VarElem *)elem)->get_name()->get_string(), 
                ((VarElem *)elem)->get_type()->get_string()
            );
        }
        else {
            printf("METHOD <CLASS:%s, METHOD:%s, <RETURN:%s", 
                ((MethodElem *)elem)->get_class_name()->get_string(),
                ((MethodElem *)elem)->get_method_name()->get_string(),
                ((MethodElem *)elem)->get_method_types()->return_type->get_string()
            );
            for (int j = 0; j < ((MethodElem *)elem)->get_method_types()->formal_types.size(); j++)
                printf(", FORMAL:%s", ((MethodElem *)elem)->get_method_types()->formal_types[j]->get_string());
            printf(">>\n");
        }
    }
}
