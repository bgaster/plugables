/**
 * @module Module.ts
 * @author Benedict R. Gaster
 * @copyright Benedict R. Gaster 2016
 * @description
 */
module Module {

    declare var require: any;

    var MongoClient = require('mongodb').MongoClient
    var assert = require('assert')
    var ObjectId = require('mongodb').ObjectID
    
    const modulesCollection : string = 'modules';
    
    export interface Entry {
	_id: any
	name: string
    }

    export class DB {
	private db: any
	
	constructor(db: any) {
	   this.db = db
	}
    
	create(name, callback) {    
	    var collection = this.db.collection(modulesCollection);
	    
	    collection.insertOne(
		{ "name" : name },
		function(err, result) {
		    assert.equal(err, null);
		    callback(result);
		}
	    );
	}
	
	finds(callback: (Entry) => void) {
	    var collection = this.db.collection(modulesCollection);
	    var cursor = collection.find();
	    
	    cursor.toArray(function(err:any, modules : Array<Entry>) {
		assert.equal(err, null);
		callback(modules);
	    });
	}

	/**
	 * @method find
	 * @description query a particular module
	 */
	find(id, callback) {
	    var collection = this.db.collection(modulesCollection);
	    var cursor = collection.findOne(
		{_id: id},
		function(err, module) {
	    	    assert.equal(err, null);
	     	    callback(module);
		});
	}

	remove(id, callback) {
	    var collection = this.db.collection(modulesCollection)
	    collection.remove( { _id: id }, function(err, results) {
		    assert.equal(err, null);
		    callback();
	    });
	}
    }
}
