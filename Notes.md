In Lisp we can sometimes use closures as a representation.

Within a closure, variable bindings can store information, and can also play the role that pointers play in constructing complex data structures.

By making a group of closures which share bindings, or can refer to one another, we can create hybrid objects which combine the advantages of data structures and programs.

Beneath the surface, shared bindings are pointers.
Closures just bring us the convenience of dealing with them at a higher level of abstraction.

By using closures to represent something we would otherwise represent with static data structures, we can often expect substantial improvements in elegance and efficiency.


Closures have three useful properties: they are active, they have local state, and we can make multiple instances of them.

 Where could we use multiple copies of active objects with local state? In applications involving networks, among others.

In many cases we can represent nodes in a network as closures. As well as having its own local state, a closure can refer to another closure.
 Thus a closure representing a node in a network can know of several other nodes (closures) to which it must send its output.
 This means that we may be able to translate some networks straight into code.
