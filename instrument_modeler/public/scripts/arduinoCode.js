/**
 * @name arduinoCode.js
 * @author Benedict R. Gaster
 * @description code generaator for Arduino Nao.
 *
 * An instrument is made from a collection of modules, where each
 * module represents a program to run on a specfic arduino block. To
 * this end each module is compiled to a single C++ file that is
 * intended to be loaded on to the corresponding arduino block. As these 
 * blocks can be connected together, via I2C, there has to be a single
 * master module and one or more slaves. (It is, of course, possible to
 * have just a single block system, in this case no I2C is required.
 * 
 * The contents of this file provide routines and contants for
 * building a code generator for modules, in particular, but by
 * extension instruments too.
 *
 * Most of the "real" work is done in the Arduino Blocks library, which
 * the generated code utilizes. This means that the actual code
 * generator is trivial.
 *
 */

function ArduinoCode(moduleId, moduleName) {

    // constant stuff
    this.baud = "9600"

    this.fileExtension = ".cpp"

    /**
     * @name namespace
     * @description The Block library namespace
     */
    this.namespace = "blocks"

    /**
     * @name comment
     * @description comment that should appear at the top of any and all 
     * generated C++ compilation units or heaaders.
     */
    this.comment =
	"/**\n"
	+ " * This code was automatically generate by Block Control Instruments\n"
	+ " * Copyright: Benedict R. Gaster, 2016\n"
	+ " * LICENSE: see license.txt @...\n"
	+ " */\n\n"

    /**
     * @name filePrefix
     * @description includes that should appear at the top of all generated C++
     * compilation units and headers.
     */
    this.filePrefix =
        this.comment 
	+ "#include <pin.hh>\n"
	+ "#include <controller.hh>\n"
	+ "#include <module.hh>\n\n"

    this.ro = "_ro"
    this.wo = "_wo"

    this.moduleName = moduleName
    this.moduleId   = moduleId

    this.controllerId = 1;
    
    /**
     * @property code
     * @description code for module instance
     * 
     * note that due to the static design of the C++ Block library, there
     * can only be one instance of a module per arduino block.
     */
    this.code =
	this.comment
	+ this.filePrefix
	+ `${this.namespace}::module<\n`
        + `    ${this.baud}, \n`
    	+ `    ${this.moduleId}, \n`
}

/**
 * @function fileEndModule
 * @description code to end a module instance
 */
ArduinoCode.prototype.fileEndModule = function () {
    return `> ${this.moduleName};\n`
}

/**
 *
 * note 
 */
ArduinoCode.prototype.fileSetup = function () {
    return "void setup()\n"
	+ "{\n"
	+ `    ${this.moduleName}.setup();\n`
	+ "}\n"
}

ArduinoCode.prototype.fileLoop = function () {
    return "void loop (void)\n"
	+ "{\n"
	+ `    ${this.moduleName}.run();\n`
	+ "}\n"
}

ArduinoCode.prototype.filePostfix = function () {
    return this.fileSetup(this.moduleName)
	+ "\n"
	+ this.fileLoop(this.moduleName)
}

ArduinoCode.prototype.createFileName = function () {
  return this.moduleName + "_" + this.moduleId + this.fileExtension
}

ArduinoCode.prototype.close = function () {
    return this.code + this.fileEndModule() + this.filePostfix()
}

/**
 * @function convertJSONControllerPinToArduinoPin
 * @description
 *
 * Note we assume that pins are one of the following forms:
 *
 *             A[0..9]+
 *             D[0..9]+
 *             N/A
 *
 * This should be enforced by their representation in the DB
 */
ArduinoCode.prototype.convertJSONControllerPinToArduinoPin = function (pin) {
    switch(pin.charAt(0)) {
    case 'N':
	return "N/A";
	break;
    case 'A':
	return this.namespace + "::a" + pin.substr(1);
	break;
    case 'D':
	return this.namespace + "::d" + pin.substr(1);
	break;
    default:
	return "unknown JSONControllerPin"
	break;

    }
}

/**
 * @function convertJSONControllerTypePinsToArduinoTypePins
 * @description
 *
 */
ArduinoCode.prototype.convertJSONControllerTypePinsToArduinoTypePins = function (
    type, pin1, pin2, pin3) {

    var str = "";
    
    switch(type) {
    case "linearPot":
	str = this.namespace + "::controller_type::LINEAR_POT,\n        "
	    + this.convertJSONControllerPinToArduinoPin(pin1) + this.ro;
	break;
    case "linearSlider":
	str = this.namespace + "::controller_type::LINEAR_SLIDER,\n        "
 	    + this.convertJSONControllerPinToArduinoPin(pin1) + this.ro;
	break;
    case "togglePair":
	str = this.namespace + "::controller_type::TOGGLE_PAIR,\n"
 	    + this.convertJSONControllerPinToArduinoPin(pin1) + this.ro + ",\n        "
 	    + this.convertJSONControllerPinToArduinoPin(pin2) + this.ro + ",\n        "
	    + this.convertJSONControllerPinToArduinoPin(pin3) + this.wo;
	break;
    case "switchTripple":
	str = this.namespace + "::controller_type::SWITCH_PAIR,\n        "
	    + this.convertJSONControllerPinToArduinoPin(pin1) + this.ro + ",\n        "
	    + this.convertJSONControllerPinToArduinoPin(pin2) + this.ro;
	break;
    default:
	// should not get here, but just in case
	return "unknown JSONControllerType"
	break;
    }
    
    return str;
}

ArduinoCode.prototype.pushController = function (id, type, pin1, pin2, pin3) {
    if (this.controllerId != 1) {
	this.code += ",\n"
    }
    this.code +=
	  `    ${this.namespace}::controller<${this.controllerId},\n`
	+ "        "
        + this.convertJSONControllerTypePinsToArduinoTypePins(
	    type,
	    pin1,
	    pin2,
	    pin3)
	+ ">"

    this.controllerId++
}
