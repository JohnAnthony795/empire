#!/usr/bin/python2.7
import os
import socket
import sys

import algos
import behavior_tree
import communication
import continent
import influence
import parameters
import parser
import situation
import stats
import tools
import units

do_debug = False

if len(sys.argv) != 3:
	print >> sys.stderr, "usage: %s <server-name> <server-port>" % sys.argv[0]
	print >> sys.stderr, "\n"
	sys.exit(1)

server_name = sys.argv[1]
server_port = int(sys.argv[2])

# Connect to the server.
server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
try:
	server.connect((server_name, server_port))
except:
	print "unable to connect"
	sys.exit(1)

server_fd = server.makefile()

the_situation = situation.Situation()
the_parameters = parameters.Parameters(the_situation)
the_continent = continent.Continent(the_situation)
the_units = units.Units()
the_influence = influence.Influence(the_situation, the_units, the_parameters)
the_stats = stats.Stats(the_situation, the_continent)
the_parser = parser.Parser(the_situation, [the_continent, the_units, the_parameters])
the_communication = communication.Communication(the_parser, server, server_fd)
the_factory = behavior_tree.Factory()

turn = 0
while 1:
	turn = turn + 1
	the_communication.wait()
	the_parameters.update()
	the_units.next_turn()
	the_influence.update()
	the_stats.update()
	the_continent.update()
	#the_situation.check()

	tools.debug("turn: %d" % turn)
	tools.debug("nb cities: %d" % len(the_situation.player_cities))
	tools.debug("nb pieces: %d" % len(the_situation.player_pieces))
	if do_debug: the_communication.action("dump_map")
	if do_debug: the_situation.show()
	if do_debug: the_stats.show()
	if do_debug: the_units.show()
	if do_debug: the_influence.show()
	if do_debug: the_continent.show()

	# 1. Process cities.
	for city in the_situation.player_cities.values():
		if city.production is not None:	
			continue
		#print "CITY: ", city.city_id
		# Follow the decision tree to get candidate pieces.
		group = the_continent.get_group(city.get_location())
		stats = dict(the_stats.group_stats[group])
		stats.update(the_stats.city_stats[city.city_id])
		candidates = the_parameters.production_decision.eval(stats)
		# Choose the piece type to produce.
		city.production = tools.weighted_choice(candidates)
		city_id = city.city_id
		the_communication.action("set_city_production %d %d" % (city_id, city.production))
	# 2. Process pieces.
	for piece in the_situation.player_pieces.values():
		# We must check if the piece already exists.
		# In fact, a transport may move on an enemy and the transported pieces are deleted!
		if the_situation.is_player_piece(piece.piece_id):
			if piece.behavior is None:
				the_info = behavior_tree.Info(the_situation, the_communication, the_continent, the_stats, the_influence, the_parameters, the_factory)
				the_info.piece = piece
				piece_behavior = "move@%d" % piece.piece_type_id
				piece.behavior = the_factory.create(the_info, piece_behavior, the_parameters.behaviors)
				piece.role = piece_behavior
			piece_type = the_situation.piece_types[piece.piece_type_id]
			piece.steps_left = piece_type.speed
			#print "piece id=%d type=%d @" % (piece.piece_id, piece.piece_type_id), piece.get_location(), " can move %d times" % piece.steps_left
			#a = piece.steps_left
			piece.behavior.act()
			#b = piece.steps_left
			#if a == b:
			#	print "  piece id=%d type=%d did not move!"
	# 3. End turn.
	the_communication.end_turn()
