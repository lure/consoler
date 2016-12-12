## Console drawing application

**How to run**
1. make sure you have apache maven and java 8 installed. 
2. cd to `consoler` directory, `mvn clean package`
3. run application with `java -jar  ./target/consoler-1.0-jar-with-dependencies.jar`
 
By default, two jar are built: one without any deps, and one with everything required to run.

**Description**
 
The application allows to test simple drawing scenarios, operating with 3 tools: line, square and bucket fill. 
Lines are limited to horizontal and vertical only. 
First of all user should define a canvas, for example: `c 10 10` -- this will crate a canvas 10*10 points. 
Maximum canvas size is limited o 32767 to 32767 points and even this size should not be used unless you really want to hang 
your computer. 

For addition info see descr1, descr2 pngs

**Some explanations** 

Speaking of canvas, the fastest solution would be to use awt or javafx (any other) graphical library, but there is no challenge 
and place to fail badly. Color in such solution could be coded by dictionary [awt.Color, Char]. 

Array of Array of Char used in this implementation is a trade off between desire to provide as much as possible 'colors' and 
the simple reason that Char is only 16b agains 32b of Integer. Also, no guarantee that user's console would allow to draw even 
UTF-8 characters.

The code is pretty straightforward in other parts except may be some mutable parts. While functional design wants us 
 to stick with immutability, GC doesn not share our ideas and reacreating arrays every time would stop the world eventually.
 
There is shapeless library used in the code. The reason is that it offer a way to build functionset and conveyor the params through it. Of course there was some drawbacks and disputable moments but 
 I have to leave something to speak with interviewer anyway.
  
Any shorter solution? Of course. Use scopt + awt + herbsy + FSM. Small amount of codes, pack of libs.
   
  
