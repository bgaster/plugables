/**
 * @module Instrument.ts
 * @author Benedict R. Gaster
 * @copyright Benedict R. Gaster 2016
 * @description
 */
module Instrument {

    declare var require: any;

    var MongoClient = require('mongodb').MongoClient
    var assert = require('assert')
    var ObjectId = require('mongodb').ObjectID

    const instrumentsCollection : string = 'instruments';

    export interface Entry {
	_id: any
	name: string
	modules:Array<{channel: number, module_id: any, _id: any}>
    }
    
    export class DB {
	private db: any
	
	constructor(db: any) {
	   this.db = db
	}

	/**
	 * @method create
	 * @description create a new instrument
	 */
	create(name, callback) {    
	    var collection = this.db.collection(instrumentsCollection);

	    collection.insertOne(
		{ "name" : name, "modules": [] },
		function(err, result) {
		    assert.equal(err, null);
		    callback();
		});
	}

	/**
	 * @method finds
	 * @description query all instruments
	 */
	finds(callback) {
	    var collection = this.db.collection(instrumentsCollection);
	    var cursor = collection.find();

	    cursor.toArray(function(err, instruments) {
	    	assert.equal(err, null);
	     	callback(instruments);
	    });
	}

	/**
	 * @method find
	 * @description query an instrument
	 */
	find(id, callback) {
	    var collection = this.db.collection(instrumentsCollection);
	    var cursor = collection.findOne(
		{_id: id},
		function(err, instrument) {
	    	    assert.equal(err, null);
	     	    callback(instrument);
		});
	}

	/**
	 * @method findWithUniqueModule
	 * @description query an instrument
	 */
	findWithUniqueModule(id, callback) {
	    var collection = this.db.collection(instrumentsCollection);
	    var cursor = collection.findOne(
		{ "modules._id" : id},
		function(err, instrument) {
	    	    assert.equal(err, null);
	     	    callback(instrument);
		});
	}

	/**
	 * @method addModule
	 * @description add a module to instrument 
	 */
	addModule(id, chan: number, moduleId, callback) {
	    var collection = this.db.collection(instrumentsCollection);
	    
	    collection.updateOne(
		{ _id : id },
		{ $push:
		  {
		      modules:
		      {
			  channel: chan,
			  module_id: moduleId,
			  _id:  new ObjectId.ObjectID()
		      }
		  }
		},
		function(err, results) {
		    assert.equal(err, null);
		    callback();
		});
	}

	/**
	 * @method updateModule
	 * @description Update the channel for a given module in instrument
	 */
	updateModule(id, uniqueId, chan: number, callback) {
	    var collection = this.db.collection(instrumentsCollection);

	    collection.updateOne(
		{ _id : id, "modules._id": uniqueId },
		{ $set: { "modules.$.channel": chan } },
		function(err, results) {
		    assert.equal(err, null);
		    callback();
		});		
	}

	/**
	 * @method removeModule
	 * @description remove a module from a given instrument
	 */
	removeModule(id, uniqueId, callback) {
	    var collection = this.db.collection(instrumentsCollection)
	    collection.update(
		{_id : id},
		{ $pull: { modules: { _id: uniqueId } } },
		function(err, results) {
		    assert.equal(err, null);
		    callback();
		});
	}

	/**
	 * @method remove
	 * @description remove an instrument from the db
	 */
	remove(id, callback) {
	    var collection = this.db.collection(instrumentsCollection)
	    collection.remove( { _id: id }, function(err, results) {
		    assert.equal(err, null);
		    callback();
	    });
	}
    }   
}