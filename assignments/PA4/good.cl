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
        self
    };
    
    f2(x:Int): D {
        {
            let x : Int <- 3 in {
                while x < 100 loop
                    x <- x + 1
                pool;
                self;
            };
        }
    };

    f3() : Bool {
        let x:Bool in {
            x
        }
    };

    f4(x:Int, c: C, d:D, o:Object) : Object {
        self
    };

};

class E inherits C {

};

class F inherits D {

};
