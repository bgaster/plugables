/**
 * @module Instrument.ts
 * @author Benedict R. Gaster
 * @copyright Benedict R. Gaster 2016
 * @description
 */
module Arduino {

    declare var require: any;

    var MongoClient = require('mongodb').MongoClient
    var assert = require('assert')
    var ObjectId = require('mongodb').ObjectID

    const arduinoCollection     : string = 'arduino';

    export interface Board {
	_id: any
	name: string
	analog_pins:Array<string>
	digital_pins:Array<string>
    }
    
    export class DB {
	private db: any
	
	constructor(db: any) {
	   this.db = db
	}

	/**
	 * @method create
	 * @description create a board
	 */
	create(name: string,
	       analogPins:  Array<string>,
	       digitalPins: Array<string>,
	       callback) {    
	    var collection = this.db.collection(arduinoCollection);

	    collection.insertOne(
		{
		    "name" : name,
		    analog_pins: analogPins,
		    digital_pins: digitalPins
		},
		function(err, result) {
		    assert.equal(err, null);
		    callback();
		});
	}

	/**
	 * @method findBoards
	 * @description query all boards
	 */
	findBoards(callback: (Board) => void) {
	    var collection = this.db.collection(arduinoCollection);
	    var cursor = collection.find();
	    
	    cursor.toArray(function(err:any, boards : Array<Board>) {
		assert.equal(err, null);
		callback(boards);
	    });
	}

	/**
	 * @method findBoard
	 * @description query a particlar board
	 */
	findBoard(name: string, callback) {
	    var collection = this.db.collection(arduinoCollection);
	    var cursor = collection.find( { "name": name } );

	    // in general, board names would be unique, but there is no
	    // requirement for this to be the case...
	    cursor.each(function(err:any, board : Board) {
		assert.equal(err, null);
		callback(board);
	    });
	}

	/**
	 * @method removeName
	 * @description remove a board from the db
	 */
	removeName(name, callback) {
	    var collection = this.db.collection(arduinoCollection)
	    collection.remove( { "name": name }, function(err, results) {
		    assert.equal(err, null);
		    callback();
	    });
	}

	/**
	 * @method remove
	 * @description remove a board from the db
	 */
	remove(id, callback) {
	    var collection = this.db.collection(arduinoCollection)
	    collection.remove( { _id: id }, function(err, results) {
		    assert.equal(err, null);
		    callback();
	    });
	}
    }
}