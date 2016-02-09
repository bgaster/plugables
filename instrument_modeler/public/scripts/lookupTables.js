/**
 * @name lookupTables.js
 * @author Benedict R. Gaster
 * @description generator for lookup table data, which is used to map Arudino
 *              module control messages to MIDI in the host application
 *
 * To avoid reprelicating the description the details are documented in
 * the host application module MIDI.Message.
 *
 * FIXME: Should really port to TypeScript, but live is too short :-) 
 */

function LookupTables(channel) {
    this.tablePrefix  = "["
    this.tablePostfix = "]"

    this.moduleTable = "["
    this.controllerTable = "["

    this.channel = `${channel}`
    
    this.numModules     = 0;

    // used as offset value in module added 
    this.numControllers = 0;
}

/**
 * @name 
 * this method should only be called once, then all corresponding controllers 
 * added, for given module added, then next module can be added, and so on
 */
LookupTables.prototype.pushModule = function() {

    if (this.numModules > 0) {
	this.moduleTable += ","
    }

    this.moduleTable += `(${this.channel},${this.numControllers})`

    this.numModules++
}

LookupTables.prototype.pushControllers = function(cmd1, cmd2, cmd3)  {
    if (this.numControllers > 0) {
	this.controllerTable += ","
    }

    this.controllerTable += `${cmd1}`
    this.numControllers++
    
    if (cmd2 != -1) {
	this.controllerTable += ","
	this.controllerTable += `${cmd2}`
	this.numControllers++
    }

    if (cmd3 != -1) {
	this.controllerTable += ","
	this.controllerTable += `${cmd3}`
	this.numControllers++
    }
}

LookupTables.prototype.closedModules = function() {
    return this.moduleTable  + "]\n"
}

LookupTables.prototype.closedControllers = function() {
    return this.controllerTable  + "]"
}

LookupTables.prototype.closedTables = function() {
    return this.moduleTable  + "]\n" + this.controllerTable  + "]"
}

