class C {
	a : Int;
	b : Bool;
	init(x : Int, y : Bool) : C {
           {
		a <- x;
		b <- y;
		self;
           }
	};
};

Class Main {
	main():C {
	  (new C).init(1,true)
	};
};

class D inherits C {
    f1(c:C, d:D): D {
        
    };
    
    f2(): D {

    };

    f3() : Bool {

    };

    f4(x:Int, c: C, d:D, o:Object) : Object {};

};

class E inherits C {

};

class F inherits D {

};
