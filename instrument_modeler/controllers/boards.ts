/**
 * @module boards.ts
 * @author Benedict R. Gaster
 * @copyright Benedict R. Gaster 2016
 * @description express routing for boards
 */

/// <reference path="../models/Arduino.ts" />

module BoardRoutes {
    declare var require: any;

    var MongoClient = require('mongodb').MongoClient
    var assert = require('assert')
    var ObjectId = require('mongodb').ObjectID

    var express = require('express');
    export var router = express.Router();

    // GET:

    /**
     * @route GET: boards
     * FIXME: this is all broken...
     */
    router.get('/board', function (req, res) {
	var arduinoDB = new Arduino.DB(req.db)
	arduinoDB.findBoards(function (boards) {
	    res.send(boards[0]);
	});
    });  
    
    // PUT:
    
    // POST:

    // DELETE:
}
