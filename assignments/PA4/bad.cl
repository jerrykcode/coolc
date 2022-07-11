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

Class Bad inherits IO {

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
    
    errAssign() : Bad {
        {
            y <- 3;
            x <- new A;
            self;
        }
    };

    errDispatch() : Bad {
        {
            (new A).undeclaredMethod();
            (new A).f1(x);
            (new A).f6(x, self, new B); --good
            (new A).f6(x, self, new IO);
            (new A).f6();
            a <- (new A).f4(); -- good
            self.f1(); --good
            f1(); --good
            self.f2(x);
            f2(x);
            f5().f6(x, self, self); --good
            a <- (new B).f5(); --good
            self;
        }
    };

    errStaticDispatch() : Bad {
        {
            (new B)@A.f6(x, self, self); --good
            a <- (new B)@A.f5(); --good
            bb@A.f1();
            (new B)@UndeclaredClass.fun();
            (new A)@B.f();
            a <- (new B)@B.f();
            (new B)@B.f(); --good
            (new B)@A.f2(self);
            (new B)@A.undeclaredMethod();
            (new B).f5(); -- return B, good
        }
    };

    errCond(n1:Int, n2:Int) : Bad {
        {
            if a <- new A then 1 else 0 fi;
            let x:Int in x <- if a = new A then 1 else 0 fi; --good
            n1 <- if if n1 < n2 then n1 else n2 fi = n1 then n1 + 1 else n1 - 1 fi; --good
            n1 <- if if n1 < n2 then n1 else n2 fi then n1 else n2 fi;

            a <- if 
                    if if n1 < n2 then n1 else n2 fi < 0 then {
                        let tmp :Int <- if n1 < n2 then n1 else n2 fi in {
                            if tmp = n1 then n1 <- 0 else n2 <- 0 fi
                        }; n1;
                    } else n1  fi 
                    = n2
                then
                    self
                else if self = a then new A else if n1<10 then self else new B fi fi fi; --good

            a <- if 
                    if if n1 < n2 then n1 else n2 fi < 0 then {
                        let tmp :Int <- if n1 < n2 then a -- err
                            else n2 fi in {
                            if tmp = n1 then n1 <- 0 else n2 <- 0 fi
                        }; n1;
                    } else n1  fi 
                    = n2
                then
                    self
                else if self = a then new A else if n1<10 then self else new B fi fi fi;


            self;
        }
    };

    condSelfType(n1:Int, n2:Int) : SELF_TYPE { --good
        if n1 = n2 then f5() else f4()  fi
    };

    errCondSelfType(n1:Int, n2:Int) : SELF_TYPE { -- bad
        if n1 = n2 then {
            f2();
        } else {
            n1 <- 0;
            condSelfType(n1, n2);
        } fi
    };

    errCondSelfType2(n1:Int, n2:Int, n3:Int) : SELF_TYPE {
        if n1=n2 then {
            if n2=n3 then {
                if n1=n3 then self else f2() fi; --err
            }
            else {
                f5();
            }fi;
        }
        else {
            condSelfType(n1, n2);
        }fi
    };

    errSelfType() :SELF_TYPE {
        f2()
    };

    goodTypcaseSelfType(var:A) : SELF_TYPE { --good
     case var of
	 a : A => out_string("Class type is now A\n");
	 b : B => self;
	 c : C => f5();
	 d : D => out_string("Class type is now D\n");
	 e : E => condSelfType(1, 2);
	 o : Object => out_string("Oooops\n");
      esac

    };

    goodLoop(n1:Int, n2:Int, n3:Int) : Object {
        {
            while n1 < n2 loop {
                n3 <- n3/2;
                while n3 < n2 loop
                    n2 <- n2 - 1
                pool;
            }
            pool;

            while 
                while 
                    while n1<n2 
                        loop { n1 <- n1 * 2; n1; } pool 
                    < 10 loop {
                        (new IO).out_string("haha~");
                        n1 <- n1 * 2;
                        n1;
                    } pool
                    < n3 loop   {
                        n3 <- n3 / 2;
                }
            pool;
        }
    };
    
    errLoop(n1:Int, n2:Int, n3:Int) : Bad {
        {
            while while n1=n2 loop n1<-n1-1 pool loop
                (new IO).out_string("haha~")
            pool;
            self;
        }
    };

    errLet():Bad {
        {
            let y:Int, z:Int <- a in y <- x;
            let y:Int in {
                y <- new A;
                let y:Int , y : A in {
                    y <- 3
                };
            };
            a;
        }
    };

    errCalculate() : Bad {
        let x:Int <- 0, y :Int <- 1, o : Object <- new Object, b : B <- new B in {
            x <- x + o;
            y <- y - b;
            b <- x*y;
            b <- b / x + y;
            x <- x + 1 + 4 * 6 +9 - 222; --good
            x <- (x + y*2) / x + y - (x*y/10); --good
            self;
        }
    };

    errNew() : Bad {
        new CCC
    };

    f1() : Object {
        new Object
    };

    f2() : A {
        new A
    };

    f3() : A {
        self
    };

    f4() : SELF_TYPE {
        self
    };

    f5() : SELF_TYPE {
        f4()
    };

    f6(x:Int, o:Object, a:A) : A {
        a
    };

    f7(x:Int) : SELF_TYPE {
        if x = 0 then self else self fi
    };
};


class B inherits A {
    f() : Object {
        self
    };
};
