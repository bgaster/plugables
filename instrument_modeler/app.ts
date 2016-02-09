/**
 *
 */

/// <reference path="./models/Instrument.ts" />
/// <reference path="./models/Module.ts" />
/// <reference path="./models/Controller.ts" />
/// <reference path="./models/Arduino.ts" />

/// <reference path="./controllers/instruments.ts" />
/// <reference path="./controllers/modules.ts" />
/// <reference path="./controllers/controllers.ts" />
/// <reference path="./controllers/boards.ts" />

declare var require: any;

var MongoClient = require('mongodb').MongoClient
var assert = require('assert')
var ObjectId = require('mongodb').ObjectID

var logger = require('morgan')

var instrumentRoutes = InstrumentRoutes.router
var moduleRoutes = ModuleRoutes.router
var controllerRoutes = ControllerRoutes.router
var boardRoutes = BoardRoutes.router

var express = require('express')
var app = express();

var bodyParser = require('body-parser')

//------------------------------------------------------------------------------

// Connection URL, our database
var url : string = 'mongodb://localhost:27017/surfaces';

//------------------------------------------------------------------------------

// use connect method to connect to the Server
MongoClient.connect(url, function(err, db) {
    assert.equal(null, err);

    app.use(bodyParser.json());         // to support JSON-encoded bodies
    app.use(bodyParser.urlencoded({     // to support URL-encoded bodies
	extended: true
    })); 

    
    // add headers
    app.use(function (req, res, next) {
	// Website you wish to allow to connect
	res.setHeader(
	    'Access-Control-Allow-Origin',
	    'null');
	
	// Request methods you wish to allow
	res.setHeader(
	    'Access-Control-Allow-Methods',
	    'GET, POST, OPTIONS, PUT, PATCH, DELETE');

	// Request headers you wish to allow
	res.setHeader(
	    'Access-Control-Allow-Headers',
	    'X-Requested-With,content-type');
	
	// Set to true if you need the website to include cookies in the requests sent
	// to the API (e.g. in case you use sessions)
	res.setHeader('Access-Control-Allow-Credentials', true);
	
	// Pass to next layer of middleware
	next();
    });

    // dump routing log...
    app.use(logger('dev'));

    // install database in local scope of routing
    app.use(function(req,res,next){
	req.db = db;
	next();
    });

    // install routing
    app.use('/', instrumentRoutes);
    app.use('/', moduleRoutes);
    app.use('/', controllerRoutes);
    app.use('/', boardRoutes);

    /// catch 404 and forwarding to error handler
    app.use(function(req, res) {
	res.writeHead(404, { "Content-Type": "text/plain" });
	res.end("404 error!\n");
    });
    
    app.listen(3000, function () {
	console.log('Example app listening on port 3000!');
    });
});
