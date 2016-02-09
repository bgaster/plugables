Instrument Modeller
===================

This is a simple web application that can be used to design plugable
control surfaces. It is part of the Plugable Control Surface Toolkit (PCST).

The application allows instruments to be desgined consisting of
modules. Each module has a programmable number of controllers,
e.g. linear pot (which might be used to control volume), that are
customized to correspond to the physical controllers they
represent. In particular, each module consists of:

* A Midi Channel
* Some number of controllers

Each controller has assignable properties, including:

* Name
* Type (e.g. linearSlider)
* Pins, analog or digital pin assignment for board target (curretly only Arduino Nano supported)
* Midi Control Commands

From any module surface it is possible to generate the corresponding
C++ code (automatically) to run on the Arduino Nano. The generated
code is straightforward as it depends on the low-level C++ libary
provided as part of the Plugable Control Surface Toolkit.

Installation 
============

The web application component is written using Typescript, in some
cases pure Javascript, HTML, and CSS. It is a pretty simply
application, split into client side, HTML, CSS, and Javascipt, and a
backend component built on Node JS, using Typescript and MongoDB.

For the client side code base, which is in the directory pubic, all
dependencies are installed with bower, i.e bower install in the public
directory.

For the backend you need to install MongoDB and Node JS. Then run npm
install in the applications root directory (i.e. this one.)

Running
=======

Start MongoDB with a path to a database. The simply run npm start in
the root directory of the application, it will compile app.ts, and use
Node to execute the resulting app.js.

Then to run the client side application simply load the page
index.html in directory public.