#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>  
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"
#include <map>
#include <string>
#include <cstring>

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

struct cmp_str_st {
    bool operator() (char *a, char *b) const {
        return strcmp(a, b) < 0;
    }
};

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
private:
  int semant_errors;
  void install_basic_classes();
  ostream& error_stream;
  std::vector<Class_> basic_classes;

  std::map<char *, int, cmp_str_st> class2id;
  std::vector<int> *graph;
  int num_vertices;
public:
  ClassTable(Classes);
  ~ClassTable();
  bool check_inheritance(Classes);
  bool is_subclassof(std::string, Symbol);
  void init_methods_info(Classes classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);

private:
  bool check(std::vector<Class_> all_classes);
};

#endif

