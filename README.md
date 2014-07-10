## CL-ACE — Lisp bindings for Attempto Tools

### Overview

CL-ACE is a set of Lisp bindings for some of the [Attempto Tools](http://attempto.ifi.uzh.ch/site/tools/). CL-ACE currently
provides bindings for the webservice version of APE (ACE Parser) and RACE (ACE Reasoner).
CL-ACE uses [DRAKMA](http://weitz.de/drakma/) as an HTTP client, and [Closure XML](http://common-lisp.net/project/cxml/)
for XML processing. CL-ACE supports parsing the results of the webservices into Lisp objects, but also provides and
interface for retrieving unprocessed webservice output. Of particular note is that CL-ACE implements CLOS classes
corresponding to the discourse representation structures described in the [ACE 6.5 DRS Report](http://attempto.ifi.uzh.ch/site/pubs/papers/drs_report_65.pdf).

### Documentation

CL-ACE has documentation (generated by [CLDOC](http://common-lisp.net/project/cldoc/)). Good starting points for
using APE are `ape-webservice:ape` and `ape-webservice:invoke-ape-webservice`. For using RACE, see 
`race-webservice:race` and `race-webservice:invoke-race-webservice`. Other important documents to read
are the APE Webservice Specification, the documentation for the Race Web Service, and other various
[Attempto Documents](http://attempto.ifi.uzh.ch/site/docs/).

