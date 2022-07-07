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

Class Bad {

};

Class RedeclaredVar inherits Bad {
    x:Int;
    x:Object;
    
    redeclaredFormals(y:Int, y:Object): Object {
        new C    
    };

    z : Int;
    good(z:Int) : Object {
        (new IO).out_string(z)
    };
};

Class RedeclaredMethod inherits Bad {
    o:Object;
    func() : Object {

    };
    func(x:Int) : Int {

    };
};

Class UndeclaredVar inherits Bad {
    main() : Object {
        x <- 3
    };
};

Class UndeclaredMethod inherits Bad {
    main() : Object {
        (new C).undeclaredMethod()
    };
};

Class A inherits Bad {
    a:A;
    x:Int;
    f() : Object {
        x <22 
    };
    main(y:Int):Bad {
        {
            x < z;
            a;
        }
    };

    errLet():Bad {
        {
            let y:Int, z:Int <- a in y <- x;
            a;
        }
    };

    errNew() : Bad {
        new CCC
    };
};
