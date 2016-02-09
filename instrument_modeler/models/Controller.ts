/**
 * @module Instrument.ts
 * @author Benedict R. Gaster
 * @copyright Benedict R. Gaster 2016
 * @description
 *
 * We keep controllers in their own collection, however, really there
 * is a very tight relationship between modules and their
 * controllers. This is highlighted by the fact that every controller
 * has a corresponding module id, making a controller unique to a
 * given module, i.e. (controller id, module id) uniquely determines a
 * controller.
 */
module Controller {

    declare var require: any;

    var MongoClient = require('mongodb').MongoClient
    var assert = require('assert')
    var ObjectId = require('mongodb').ObjectID

    const controllersCollection : string = 'controllers';

    export interface ModuleController {
	_id: any
	module_id: any
	type_id: any
	name: string
	pin1: string
	pin2: string
	pin3: string
	controlCommand1: number
	controlCommand2: number
	controlCommand3: number
    }

    export interface ControllerType {
	_id: any
	name: string
    }

    export class DB {
	private db: any

	/**
	 * @constructor 
	 * @description
	 */
	constructor(db: any) {
	   this.db = db
	}

	/**
	 * @method create 
	 * @description Create a controller for given module
	 */
	create(
	    moduleId: any, typeId: any,
	    name: string,
	    p1: string, cmd1: number,
	    p2: string = "N/A", cmd2: number = -1,
	    p3: string = "N/A", cmd3: number = -1,
	    callback) {    
	    var collection = this.db.collection(controllersCollection);

	    collection.insertOne(
		{ module_id: moduleId,
		  type_id: typeId,
		  "name" : name,
		  pin1: p1,
		  pin2: p2,
		  pin3: p3,
		  controlCommand1: cmd1,
		  controlCommand2: cmd2,
		  controlCommand3: cmd3
		},
		function(err, result) {
		    assert.equal(err, null);
		    callback();
		});
	}

	/**
	 * @method finds
	 * @description query all module controllers
	 */
	finds(callback: (ModuleController) => void) {
	    var collection = this.db.collection(controllersCollection);
	    var cursor = collection.find();
	    
	    cursor.toArray(
		function(err:any, controllers : Array<ModuleController>) {
		    
		assert.equal(err, null);
		callback(controllers);
	    });
	}

	/**
	 * @method find
	 * @description query a module controller
	 */
	find(id, callback) {
	    var collection = this.db.collection(controllersCollection);
	    collection.findOne(
		{ _id: id },	    
		function(err, controller) {
		    assert.equal(err, null);
		    callback(controller);
		});
	}

	
	/**
	 * @method finds
	 * @description query all controllers for a given module
	 */
	findModuleControllers(
	    moduleId: any,
	    callback: (ModuleController) => void) {
	    
	    var collection = this.db.collection(controllersCollection);
	    var cursor = collection.find( { "module_id": moduleId } );
	    
	    cursor.toArray(
		function(err:any, controllers : Array<ModuleController>) {
		    
		assert.equal(err, null);
		callback(controllers);
	    });
	}
	
	/**
	 * @method update
	 * @description update a given controller
	 *
	 * id, module_id, and name cannot be changed
	 * other required fields must be provided, even if unchanged
	 */
	update(id: any, typeId: any,
	       p1: string, cmd1: number,
	       p2: string = "N/A", cmd2: number = -1,
	       p3: string = "N/A", cmd3: number = -1,
	       callback) {
	    var collection = this.db.collection(controllersCollection);

	    collection.updateOne(
		{ _id: id },
		{ $set:
		  {
		      type_id: typeId,
		      pin1: p1,
		      pin2: p2,
		      pin3: p3,
		      controlCommand1: cmd1,
		      controlCommand2: cmd2,
		      controlCommand3: cmd3
		  }
		},
		function(err, results) {
		    assert.equal(err, null)
		    callback()
		})
	}
	
	/**
	 * @method createType
	 * @description
	 */
	createType(name, callback) {    
	    var collection = this.db.collection(controllersCollection);

	    collection.insertOne(
		{ "name" : name },
		function(err, result) {
		    assert.equal(err, null);
		    callback();
		});
	}

	/**
	 * @method findTypes
	 * @description query all controller types
	 */
	findTypes(callback: (ControllerType) => void) {
	    var collection = this.db.collection(controllersCollection);
	    // we don't want to return any modules
	    var cursor = collection.find( { "module_id": { "$exists" : false } } );
	    
	    cursor.toArray(function(err:any, controllerTypes : Array<ControllerType>) {
		assert.equal(err, null);
		callback(controllerTypes);
	    });
	}

	/**
	 * @method remove
	 * @description remove a controller or controller type from collection
	 *
	 */
	remove(id, callback) {
	    var collection = this.db.collection(controllersCollection)
	    collection.remove( { _id: id }, function(err, results) {
		    assert.equal(err, null);
		    callback();
	    });
	}
    }
}