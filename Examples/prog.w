// The skeleton parser accepts only print and empty statements
;;;;;
print "Testing...\n";
;;;;;
var a = 1;
var b = a;
a = a -2; 
print a;
print " ";
print b;
print "\n";

if(a == b){
	print "yes \n";
}else{
	print "no \n";
}
if(a != b){
	print "yes \n";
}else{
	print "no \n";
}
if(a <= b){
	print "yes \n";
}else{
	print "no \n";
}
if(a >= b){
	print "yes \n";
}else{
	print "no \n";
}
if(a < b){
	print "yes \n";
}else{
	print "no \n";
}
if(a > b){
	print "yes \n";
}else{
	print "no \n";
}
while(b < a){
	b = b + 1;
	print b;
	print " ";
}
print "\n";
while(!(b < a)){
	b = b - 1;
	print b;
	print " ";
}
print "\n";

var c = "Hello";
print c;
print "\n";

var d = True;
print d;
print "\n";

if(d){
	print "yes \n";
}else{
	print "no \n";
}
{
var b = 1;
}
print "Testing...\n";
{
var b = 1;
b = 1;
}
print "Testing...\n";

print 1 + 1;
print "\n";
print True == True;
print "\n";
print a == b;
print "\n";
print a != b;
print "\n";
var n =  !(1==2);
print n;
print "\n";

{
var a = -1;
var b = 1;
var c = a-b;
print c;
print "\n";
}

if( True && False)
	print "no \n";
else
	print "yes \n";
	
	
{
var a = 2*2;
var b = a*2;
var c = a * b;
var d = c / a;
print a;
print "\n";
print b;
print "\n";
print c;
print "\n";
print d;
print "\n";
}
if( True && True)
	print "yes \n";
else{
	print "no \n";
	print "this is not fine \n";
	}
print "this is fine \n";

var f = 1;
while(f < 5)
	f = f+1;
print f;
print "\n";

var g = (1+2) * 3 / 9 - (2*1+4/2) * 7 == -27;
print g;

var h = !(d && d);
print h;
	






