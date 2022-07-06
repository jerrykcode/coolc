

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdbool.h>
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

ClassTable::~ClassTable() {
    std::vector<Class_>().swap(basic_classes);
    std::map<const char *, int, cmp_str_st>().swap(class2id);
    for (int v = 0; v < num_vertices; v++)
        std::vector<int>().swap(graph[v]);
}

void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes bui
    //          if x < y then 5 else 5 fi;lt below.
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


PCT class_table;
PSYMT symbol_table;
Symbol current_class_name;
std::map<char *, int, cmp_str_st> _str2id;
int _id;
std::map<char *, PMethodInfo, cmp_str_st> method_table;
std::vector<Type *> all_types_pool; //Use for delete memory

inline void init(Classes classes) {
    class_table = new ClassTable(classes);
    symbol_table = new SymbolTable<int, Type>();
    _id = 0;
}

inline Type *new_type(Symbol type, int line_number) {
    if (_str2id.find(type->get_string()) == _str2id.end())
        _str2id[type->get_string()] = _id++;

    int type_id = _str2id[type->get_string()];
    Type *res = new Type(type_id, type->get_string(), line_number);
    all_types_pool.push_back(res);
    return res;
}

inline Type *unknown_type() {
    static Type *singleton = NULL;
    if (singleton == NULL) {
        char *str = "unknown";
        _str2id[str] = _id++;
        singleton = new Type(_id - 1, str, 0);
        all_types_pool.push_back(singleton);
    }
    return singleton;
}

inline Type *void_type() {
    static Type *singleton = NULL;
    if (singleton == NULL) {
        char *str = "void";
        _str2id[str] = _id++;
        singleton = new Type(_id - 1, str, 0);
        all_types_pool.push_back(singleton);
    }
    return singleton;
}

inline Type *no_expr_type() {
    static Type *singleton = NULL;
    if (singleton == NULL) {
        char *str = "no_expr";
        _str2id[str] = _id++;
        singleton = new Type(_id - 1, str, 0);
        all_types_pool.push_back(singleton);
    }
    return singleton;
}

inline Type *bool_type() {
    static Type *singleton = NULL;
    if (singleton == NULL) {
        char *str = "Bool";
        _str2id[str] = _id++;
        singleton = new Type(_id - 1, str, 0);
        all_types_pool.push_back(singleton);
    }
    return singleton;
}

inline Type *int_type() {
    static Type *singleton = NULL;
    if (singleton == NULL) {
        char *str = "Int";
        _str2id[str] = _id++;
        singleton = new Type(_id - 1, str, 0);
        all_types_pool.push_back(singleton);
    }
    return singleton;
}

inline Type *string_type() {
    static Type *singleton = NULL;
    if (singleton == NULL) {
        char *str = "String";
        _str2id[str] = _id++;
        singleton = new Type(_id - 1, str, 0);
        all_types_pool.push_back(singleton);
    }
    return singleton;
}

inline void addid(Symbol name, Symbol type, int line_number) {
    if (_str2id.find(name->get_string()) == _str2id.end())
        _str2id[name->get_string()] = _id++;
    if (_str2id.find(type->get_string()) == _str2id.end())
        _str2id[type->get_string()] = _id++;

    int name_id = _str2id[name->get_string()];
    int type_id = _str2id[type->get_string()];

    Type *previous = symbol_table->probe(name_id);
    if (previous != NULL)
        class_table->semant_error() << "Line " << line_number << ": \'" << name->get_string() << "\' redeclared." << endl
          << "note: previous declaration of \'" << name->get_string() << "\' was in Line " << previous->get_line_number() << endl;
    else {
        Type *newtype = new Type(type_id, type->get_string(), line_number);
        all_types_pool.push_back(newtype);
        symbol_table->addid(name_id, newtype);
    }
}

inline Type *lookup(Symbol name) {
    if (_str2id.find(name->get_string()) == _str2id.end())
        return NULL;
    return symbol_table->lookup(_str2id[name->get_string()]);
}

inline void add_method(Symbol class_name, Symbol method_name, PMethodInfo method_info) {
    char *class_str = class_name->get_string();
    char *method_str = method_name->get_string();

    int class_str_len = strlen(class_str);
    char *key = new char[class_str_len + strlen(method_str) + 1];
    strcpy(key, class_str);
    strcpy(key + class_str_len, method_str);

    if (method_table.find(key) != method_table.end()) {
        class_table->semant_error() << "Line " << method_info->line_number << ": redeclaration of method \'"
            << class_name->get_string() << "@" << method_name->get_string() << "\'." << endl;
        delete key;
    }
    else method_table[key] = method_info;
}

inline PMethodInfo get_method(Type *caller_type, Symbol method_name) {
    std::string& class_str = caller_type->get_type_str();
    char *method_str = method_name->get_string();
    int key_len = class_str.size() + strlen(method_str) + 1;
    char *key = new char[key_len];
    int i;
    for (i = 0; i < class_str.size(); i++) key[i] = class_str[i];
    strcpy(key + i, method_str);
    PMethodInfo result = method_table[key];
    delete key;
    return result;
}

inline void print_method_info(PMethodInfo info) {
    printf("<ret: %s  formals:<", info->return_type->get_string());
    for (int i = 0; i < info->formal_types.size(); i++) printf(" %s", info->formal_types[i]->get_string());
    printf(" >>\n");
}

void program_class::semant()
{
  initialize_constants();

    init(classes);

    /* some semantic analysis code may go here */
    // assignment PA4

    if (class_table->check_inheritance(classes)) {
        class_table->init_methods_info(classes);
    }

    for (auto it = method_table.begin(); it != method_table.end(); it++) {
        print_method_info(it->second);
    }

    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        classes->nth(i)->type_check();
    }


    if (class_table->errors()) {
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
    }
}

bool ClassTable::check_inheritance(Classes classes) {
    std::vector<Class_> all_classes(basic_classes); //copy
    for (int i = classes->first(); classes->more(i); i = classes->next(i))
        all_classes.push_back(classes->nth(i));
    bool result = true;
    if (!check(all_classes)) {
        result = false;
        semant_errors++;
        cerr << "Inheritance Error!~~" << endl;
    }
    std::vector<Class_>().swap(all_classes);
    return result;
}

bool ClassTable::check(std::vector<Class_>& all_classes) {
    int cnt = 0;
    for (int i = 0; i < all_classes.size(); i++) {
        Class_ class_ = all_classes[i];
        // mapping from Class_ to id
        if (class2id.find(class_->get_name()->get_string()) == class2id.end()) 
           class2id[class_->get_name()->get_string()] = cnt++;
    }
    num_vertices = cnt;
    graph = new std::vector<int>[num_vertices];
    int *indegree = new int[num_vertices];
    memset(indegree, 0, sizeof(int)*num_vertices);
    // Insert edges
    for (int i = 0; i < all_classes.size(); i++) {
        Class_ class_ = all_classes[i];
        int class_id = class2id[class_->get_name()->get_string()];
        int parent_id = class2id[class_->get_parent()->get_string()];
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
    delete indegree;
    return cnt == num_vertices;
}

inline char *string2charptr(std::string& s) {
    int len = s.size();
class_table->semant_error() << len << endl;
int *testi = new int[len + 1];
    char *cs = new char[len + 1];
class_table->semant_error() << "b" << endl;
    for (int i = 0; i < len; i++)
        cs[i] = s[i];
class_table->semant_error() << "c" << endl;
    cs[len] = 0;
class_table->semant_error() << "e" << endl;
    return cs;
}

bool ClassTable::is_subclassof(std::string& sub_class_str, std::string& class_str) {
    char *sub_class_cstr = string2charptr(sub_class_str);
    char *class_cstr = string2charptr(class_str);
    bool res = is_subclassof(sub_class_cstr, class_cstr);
    delete sub_class_cstr;
    delete class_cstr;
    return res;
}

bool ClassTable::is_subclassof(const char *sub_class_cstr, const char *class_cstr) {
    if (strcmp(sub_class_cstr, class_cstr) == 0)
        return true;
    if (class2id.find(sub_class_cstr) == class2id.end()) {
        return false;
    }
    int sub_class_id = class2id[sub_class_cstr];
    if (class2id.find(class_cstr) == class2id.end()) {
        return false;
    }
    int class_id = class2id[class_cstr];

    // bfs check if there is a route from sub_class_id to class_id
    std::queue<int> qq;
    qq.push(sub_class_id);
    //bool *visit = new bool[num_vertices];
    int *visit = new int[num_vertices];
    memset(visit, 0, num_vertices*sizeof(int));
    visit[sub_class_id] = 1; 
    bool result = false;
    while (!qq.empty()) {
        int v = qq.front();
        qq.pop();
        for (auto it = graph[v].begin(); it != graph[v].end(); it++) {
            if (*it == class_id) {
                result = true;
                break;
            }
            if (!visit[*it]) {
                visit[*it] = 1;
                qq.push(*it);
            }
        }
        if (result) break;
    }
    while (!qq.empty()) qq.pop();
    //std::queue<int>().swap(qq);
    delete visit;
    return result;
}

Type *ClassTable::lca(Type *a, Type *b) {
    if (a->get_type_id() == b->get_type_id()) //same type
        return a;
    int **dist = new int*[2];
    std::queue<int> qq;
    char *a_cstr = string2charptr(a->get_type_str());
    if (class2id.find(a_cstr) == class2id.end()) {
        delete dist;
        std::queue<int>().swap(qq);
        delete a_cstr;
        return NULL;
    }
    int a_id = class2id[a_cstr];
    delete a_cstr;
    char *b_cstr = string2charptr(b->get_type_str());
    if (class2id.find(b_cstr) == class2id.end()) {
        delete dist;
        std::queue<int>().swap(qq);
        delete a_cstr;
        delete b_cstr;
        return NULL;
    }
    int b_id = class2id[b_cstr];
    delete b_cstr;
    int src, v, *d;
    for (src = a_id, d = dist[0];  ; src = b_id, d = dist[1]) { // only loop 2 times
        d = new int[num_vertices];
        memset(d, 0xff, num_vertices * sizeof(int)); //fill with -1
        d[src] = 0;
        qq.push(src);
        while (!qq.empty()) {
            v = qq.front();
            qq.pop();
            for (auto it = graph[v].begin(); it != graph[v].end(); it++) {
                if (d[*it] == -1) { // not visit yet
                    d[*it] = d[v] + 1;
                    qq.push(*it);
                }
            }
        }
        if (src == b_id) break;
    }
    std::queue<int>().swap(qq);
    int lca_id = -1;
    const char *lca_cstr;
    int id;
    for (auto it = class2id.begin(); it != class2id.end(); it++) {
        id = it->second;
        if (dist[0][id] != -1 && dist[1][id] != -1)
            if (dist[0][id] == dist[1][id] && (lca_id == -1 || dist[0][id] < lca_id)) {
                lca_id = id;
                lca_cstr = it->first;
            }
    }
    delete dist[0];
    delete dist[1];
    delete dist;
    if (lca_id != -1) {
        Type *res = new Type(lca_id, lca_cstr, -1/* not very clear what a line_number should be in here... */);
        all_types_pool.push_back(res);
        return res;
    }
    return NULL;
}

Symbol class__class::get_name() {
    return name;
}

Symbol class__class::get_parent() {
    return parent;
}

void ClassTable::init_methods_info(Classes classes) {
    std::vector<Class_> all_classes(basic_classes); //copy
    for (int i = classes->first(); classes->more(i); i = classes->next(i))
        all_classes.push_back(classes->nth(i));

    for (Class_ class_ : all_classes)
        class_->init_methods_info();

    std::vector<Class_>().swap(all_classes);
}

void class__class::init_methods_info() {
    method_class *method;
    for (int i = features->first(); features->more(i); i = features->next(i))
        if (features->nth(i)->is_method()) {
            method = (method_class *)features->nth(i);
            method->init_info(name);
        }
}

void method_class::init_info(Symbol class_name) {
    PMethodInfo method_info = new struct MethodInfo();
    for (int i = formals->first(); formals->more(i); i = formals->next(i))
        formals->nth(i)->add_type(method_info->formal_types);
    method_info->return_type = return_type;
    method_info->line_number = line_number;
    add_method(class_name, name, method_info);
}

void formal_class::add_type(std::vector<Symbol>& types) {
    types.push_back(type_decl);
}

bool method_class::is_attribute() { return false; }
bool method_class::is_method() { return true; }
bool attr_class::is_attribute() { return true; }
bool attr_class::is_method() { return false; }


///
//type check
//

inline bool is_unknown(Type *type) {
    return type->get_type_id() == unknown_type()->get_type_id();
}

inline bool is_bool(Type *type) {
    return type->get_type_id() == bool_type()->get_type_id();
}

inline bool is_int(Type *type) {
    return type->get_type_id() == int_type()->get_type_id();
}

inline bool is_string(Type *type) {
    return type->get_type_id() == string_type()->get_type_id();
}

inline bool is_no_expr(Type *type) {
    return type->get_type_id() == no_expr_type()->get_type_id();
}

bool class__class::type_check() {
    bool result = true;
    current_class_name = name;
    symbol_table->enterscope();
    for (int i = features->first(); features->more(i); i = features->next(i))
        if (features->nth(i)->is_attribute()) {
           (( attr_class *)features->nth(i))->add_to_symbol_table();
        }
    for (int i = features->first(); features->more(i); i = features->next(i))
        if (features->nth(i)->is_method()) {
            if ( ! ((method_class *)features->nth(i))->type_check() ) result = false;
        }
    symbol_table->exitscope();
    return result;
}


void attr_class::add_to_symbol_table() {
    addid(name, type_decl, line_number);
}


bool method_class::type_check() {
    symbol_table->enterscope();
    int n_errors = class_table->errors();
    for (int i = formals->first(); formals->more(i); i = formals->next(i)) 
        formals->nth(i)->add_to_symbol_table();
    Type *expr_type = expr->type_check();
class_table->semant_error() << "0" << endl;
    char *expr_type_cstr = string2charptr(expr_type->get_type_str());
class_table->semant_error() << "1: " << expr_type_cstr
 << "  " << return_type->get_string() << endl;

    if (!class_table->is_subclassof(expr_type_cstr, return_type->get_string())) {
class_table->semant_error() << "1.1" << endl;
        char *no_expr_cstr = string2charptr(no_expr_type()->get_type_str());
        char *void_cstr = string2charptr(void_type()->get_type_str());
        
        class_table->semant_error() << "Line " << line_number << ": in method \'" << name->get_string()
            << "\': incompatible types when returnning type \'" 
            << (strcmp(expr_type_cstr, no_expr_cstr) ? expr_type_cstr : void_cstr)
            << "\' but \'" << return_type->get_string() << "\' was expected" << endl;

        delete no_expr_cstr;
        delete void_cstr;
    }
class_table->semant_error() << "2" << endl;

    delete expr_type_cstr;

    symbol_table->exitscope();
    return n_errors == class_table->errors();
}

void formal_class::add_to_symbol_table() {
    addid(name, type_decl, line_number);
}


Type *assign_class::type_check() {
    // name <- expr
    Type *var_type = lookup(name);
    if (var_type == NULL)
        class_table->semant_error() << "Line " << line_number << ": \'" << name->get_string() << "\' undeclared!" << endl;

    Type *expr_type = expr->type_check();
    if (var_type) {
        if (class_table->is_subclassof(expr_type->get_type_str(), var_type->get_type_str())) {
            if (expr_type->get_type_id() != var_type->get_type_id()) {
                symbol_table->addid(_str2id[name->get_string()], expr_type);
            }
        }  
        else
            class_table->semant_error() << "Line " << line_number << ": incompatible types when assigning to type \'"
                << var_type->get_type_str() << "\' from type \'" << expr_type->get_type_str() << "\'" << endl;
    }
    return var_type ? var_type : expr_type;
}

inline bool str_equal(std::string& str, char *cstr) {
    int len = str.size();
    if (len != strlen(cstr)) return false;
    for (int i = 0; i < len; i++)
        if (str[i] != cstr[i]) return false;
    return true;
}

Type *dispatch_class::type_check() {
    Type *caller_type = expr->type_check();
    if (caller_type == NULL) {
      return NULL;
    }
    PMethodInfo method_info = get_method(caller_type, name);
    bool err = false;
    if (method_info == NULL) {
        class_table->semant_error() << "Line " << line_number << ": Method \'" << name->get_string()
            << "\' undeclared in class \'" << caller_type->get_type_str() << "\'" << endl;
        return unknown_type();
    }
    else {
        int i;
        for (i = actual->first(); actual->more(i); i = actual->next(i)) {
            if (i >= method_info->formal_types.size()) { err = true; break; }
            if (! str_equal(actual->nth(i)->type_check()->get_type_str(), method_info->formal_types[i]->get_string())) { 
                err = true; break; 
            }
        }
        if (i != method_info->formal_types.size()) err = true;
        if (err)
            class_table->semant_error() << "Line " << line_number << ": Method \'" << name->get_string() 
                << "\': incompatible type for argument" << endl;
    }
    return new_type(method_info->return_type, line_number);
}


Type *static_dispatch_class::type_check() {
    Type *caller_type = expr->type_check();
    char *caller_cstr = string2charptr(caller_type->get_type_str());
    if (!class_table->is_subclassof(caller_cstr, type_name->get_string())) {
        delete caller_cstr;
        class_table->semant_error() << "Line " << line_number << ": class \'" << caller_type->get_type_str()
            << "\' is not a descendant of class \'" << type_name->get_string() << "\'" << endl;
        return NULL; 
    }
    delete caller_cstr;
    PMethodInfo method_info = get_method(new_type(type_name, line_number), name);
    if (method_info == NULL) {
        class_table->semant_error() << "Line " << line_number << ": Method \'" << name->get_string()
            << "\' undeclared in class \'" << type_name->get_string() << "\'" << endl;
        return unknown_type();
    }
    else {
        bool err = false;
        int i;
        for (i = actual->first(); actual->more(i); i = actual->next(i)) {
            if (i >= method_info->formal_types.size()) { err = true; break; }
            if (! str_equal(actual->nth(i)->type_check()->get_type_str(), method_info->formal_types[i]->get_string())) {
                err = true; break;
            }
        }
        if (err)
            class_table->semant_error() << "Line " << line_number << ": Method \'" << name->get_string() 
                << "\': incompatible type for argument" << endl;
    }
    return new_type(method_info->return_type, line_number);
}

Type *cond_class::type_check() {
    Type *pred_type = pred->type_check();
    if (pred_type && ! is_bool(pred_type))
        class_table->semant_error() << "Line " << line_number << ": \'if\' condition: not a boolean expression" << endl;
    Type *then_type = then_exp->type_check();
    Type *else_type = else_exp->type_check();
    Type *lca_type = class_table->lca(then_type, else_type);

    if (lca_type == NULL) {
        class_table->semant_error() << "Line " << line_number << ": \'if\' condition: "
            << "mismatching of \'then\' and \'else\' expression" << endl;
        return then_type;
    }
    return lca_type;
}

Type *loop_class::type_check() {
    Type *pred_type = pred->type_check();
    if (!is_bool(pred_type))
        class_table->semant_error() << "Line " << line_number << ": \'while\' loop: not a boolean expression" << endl;
    body->type_check();
    return void_type();
}

Type *typcase_class::type_check() {
    Type *expr_type = expr->type_check();
    for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
        branch_class *branch = (branch_class *)cases->nth(i);
        branch->type_check();
    }
    return expr_type;
}

Type *branch_class::type_check() {
    symbol_table->enterscope();
    addid(name, type_decl, line_number);
    Type *type = expr->type_check();
    symbol_table->exitscope();
    return type;
}

Type *block_class::type_check() {
    Type *res = void_type();
    for (int i = body->first(); body->more(i); i = body->next(i)) {
        res = body->nth(i)->type_check();
    }
    return res;
}

Type *let_class::type_check() {
    symbol_table->enterscope();
    addid(identifier, type_decl, line_number);
    Type *init_type = init->type_check();
    char *init_type_cstr = string2charptr(init_type->get_type_str());
    if (!is_no_expr(init_type) && !class_table->is_subclassof(init_type_cstr, type_decl->get_string()))
        class_table->semant_error() << "Line " << line_number << ": \'let\': incompatible types when assigning type from \'"
            << init_type_cstr << "\' to \'" << type_decl->get_string() << "\'" << endl;
    delete init_type_cstr;
    body->type_check();
    symbol_table->exitscope();
    return void_type();
}  

Type *plus_class::type_check() {
    Type *e1_type = e1->type_check();
    Type *e2_type = e2->type_check();
    Type *lca_type = class_table->lca(e1_type, e2_type);
    if (lca_type == NULL)
        class_table->semant_error() << "Line " << line_number << ": no match for \'operator+\' (operand types are \'"
            << e1_type->get_type_str() << "\' and \'" << e2_type->get_type_str() << "\')" << endl;
    return lca_type ? lca_type : e1_type;
}  

Type *sub_class::type_check() {
    Type *e1_type = e1->type_check();
    Type *e2_type = e2->type_check();
    Type *lca_type = class_table->lca(e1_type, e2_type);
    if (lca_type == NULL)
        class_table->semant_error() << "Line " << line_number << ": no match for \'operator-\' (operand types are \'"
            << e1_type->get_type_str() << "\' and \'" << e2_type->get_type_str() << "\')" << endl;
    return lca_type ? lca_type : e1_type;
}  

Type *mul_class::type_check() {
    Type *e1_type = e1->type_check();
    Type *e2_type = e2->type_check();
    Type *lca_type = class_table->lca(e1_type, e2_type);
    if (lca_type == NULL)
        class_table->semant_error() << "Line " << line_number << ": no match for \'operator*\' (operand types are \'"
            << e1_type->get_type_str() << "\' and \'" << e2_type->get_type_str() << "\')" << endl;
    return lca_type ? lca_type : e1_type;
}  

Type *divide_class::type_check() {
    Type *e1_type = e1->type_check();
    Type *e2_type = e2->type_check();
    Type *lca_type = class_table->lca(e1_type, e2_type);
    if (lca_type == NULL)
        class_table->semant_error() << "Line " << line_number << ": no match for \'operator\/\' (operand types are \'"
            << e1_type->get_type_str() << "\' and \'" << e2_type->get_type_str() << "\')" << endl;
    return lca_type ? lca_type : e1_type;
}  

Type *neg_class::type_check() {
    Type *e1_type = e1->type_check();
    if (!is_int(e1_type))
        class_table->semant_error() << "Line " << line_number << ": no match for \'operator~\' (operand type is \'"
            << e1_type->get_type_str() << "\')" << endl;
    return int_type();
} 

Type *lt_class::type_check() {
    Type *e1_type = e1->type_check();
    Type *e2_type = e2->type_check();
    Type *lca_type = class_table->lca(e1_type, e2_type);
    if (lca_type == NULL || (!is_int(lca_type) && !is_string(lca_type)))
        class_table->semant_error() << "Line " << line_number << ": no match for \'operator<\' (operand types are\'"
            << e1_type->get_type_str() << "\' and \'" << e2_type->get_type_str() << "\')" << endl;
    return bool_type();
} 


Type *eq_class::type_check() {
    Type *e1_type = e1->type_check();
    Type *e2_type = e2->type_check();
    Type *lca_type = class_table->lca(e1_type, e2_type);
    if (lca_type == NULL || (!is_int(lca_type) && !is_string(lca_type)))
        class_table->semant_error() << "Line " << line_number << ": no match for \'operator=\' (operand types are\'"
            << e1_type->get_type_str() << "\' and \'" << e2_type->get_type_str() << "\')" << endl;
    return bool_type();
}  

Type *leq_class::type_check() {
    Type *e1_type = e1->type_check();
    Type *e2_type = e2->type_check();
    Type *lca_type = class_table->lca(e1_type, e2_type);
    if (lca_type == NULL || (!is_int(lca_type) && !is_string(lca_type)))
        class_table->semant_error() << "Line " << line_number << ": no match for \'operator<=\' (operand types are\'"
            << e1_type->get_type_str() << "\' and \'" << e2_type->get_type_str() << "\')" << endl;
    return bool_type();
}  

Type *comp_class::type_check() {
    Type *e1_type = e1->type_check();
    if (!is_bool(e1_type))
        class_table->semant_error() << "Line " << line_number << ": no match for \'operator!\' (operand type is \'"
            << e1_type->get_type_str() << "\')" << endl;
    return bool_type();
}  

Type *int_const_class::type_check() {
    return int_type();
}  

Type *bool_const_class::type_check() {
    return bool_type();
}  

Type *string_const_class::type_check() {
    return string_type();
} 

Type *new__class::type_check() {
    return new_type(type_name, line_number);
}  

Type *isvoid_class::type_check() {
    e1->type_check();
    return bool_type();
}  

Type *no_expr_class::type_check() {
    return no_expr_type();
} 

Type *object_class::type_check() {
    Type *res = lookup(name);
    if (res == NULL)
        class_table->semant_error() << "Line " << line_number << ": \'" << name->get_string() << "\' undeclared" << endl;
    return res ? res : unknown_type();
}  
