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
const fileExtension = ".cpp"

/**
 * @name namespace
 * @description The Block library namespace
 */
const namespace = "blocks"

/**
 * @name comment
 * @description comment that should appear at the top of any and all 
 * generated C++ compilation units or heaaders.
 */
const comment =
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
const filePrefix =
        comment 
      + "#include <pin.hh>\n"
      + "#include <controller.hh>\n"
      + "#include <module.hh>\n\n"


/**
 * @function fileStartModule
 * @description code to begin a module instance
 * 
 * note that due to the static design of the C++ Block library, there
 * can only be one instance of a module per arduino block.
 */
function fileStartModule(moduleId) {
    return `constexpr ${namespace}::module<\n`
	+ `    ${moduleId}, \n`
}

/**
 * @function fileEndModule
 * @description code to end a module instance
 */
function fileEndModule(name) {
    return `> ${name};\n`
}

/**
 *
 * note 
 */
function fileSetup(moduleName) {
    return "void setup()\n"
	+ "{\n"
	+ `    ${moduleName}.setup();\n`
	+ "}\n"
}

function fileLoop(moduleName) {
    return "int main (void)\n"
	+ "{\n"
	+ `    ${moduleName}.execute_iteration();\n`
	+ "}\n"
}

function filePostfix(moduleName) {
    return fileSetup(moduleName)
	+ "\n"
	+ fileLoop(moduleName)
}

function createFileName(moduleName, moduleId) {
  return moduleName + "_" + moduleId + fileExtension
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
function convertJSONControllerPinToArduinoPin(pin) {
    switch(pin.charAt(0)) {
    case 'N':
	return "N/A";
	break;
    case 'A':
	return namespace + "::a" + pin.substr(1);
	break;
    case 'D':
	return namespace + "::d" + pin.substr(1);
	break;
    default:
	return "unknown JSONControllerPin"
	break;

    }
}

const ro = "_ro";
const wo = "_wo";

/**
 * @function convertJSONControllerTypePinsToArduinoTypePins
 * @description
 *
 */
function convertJSONControllerTypePinsToArduinoTypePins(
    type, pin1, pin2, pin3) {

    var str = "";
    
    switch(type) {
    case "linearPot":
	str = namespace + "::controller_type::LINEAR_POT,\n        "
	    + convertJSONControllerPinToArduinoPin(pin1) + ro;
	break;
    case "linearSlider":
	str = namespace + "::controller_type::LINEAR_SLIDER,\n        "
 	    + convertJSONControllerPinToArduinoPin(pin1) + ro;
	break;
    case "togglePair":
	str = namespace + "::controller_type::TOGGLE_PAIR,\n"
 	    + convertJSONControllerPinToArduinoPin(pin1) + ro + ",\n        "
 	    + convertJSONControllerPinToArduinoPin(pin2) + ro + ",\n        "
	    + convertJSONControllerPinToArduinoPin(pin3) + wo;
	break;
    case "switchTripple":
	str = namespace + "::controller_type::SWITCH_PAIR,\n        "
	    + convertJSONControllerPinToArduinoPin(pin1) + ro + ",\n        "
	    + convertJSONControllerPinToArduinoPin(pin2) + ro;
	break;
    default:
	// should not get here, but just in case
	return "unknown JSONControllerType"
	break;
    }
    
    return str;
}

function fileController(id, type, pin1, pin2, pin3) {
    return `    ${namespace}::controller<${id},\n`
	+ "        "
        + convertJSONControllerTypePinsToArduinoTypePins(
	    type,
	    pin1,
	    pin2,
	    pin3)
	+ ">";
}
