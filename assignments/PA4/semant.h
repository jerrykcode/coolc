#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>  
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

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

public:
  ClassTable(Classes);
  bool check_inheritance(Classes);
  Environment *init_methods_info(Classes classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
};

class InheritanceChecker {
public:
  virtual bool check(std::vector<Class_> all_classes) = 0;
};

class GraphInheritanceChecker : public InheritanceChecker {
public:
  virtual bool check(std::vector<Class_> all_classes);
  GraphInheritanceChecker() {}
  ~GraphInheritanceChecker() {}
};


#endif

