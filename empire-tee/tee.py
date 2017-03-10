#!/usr/bin/python2.7

import os
import sys
import socket
import select

if len(sys.argv) != 5:
	print >> sys.stderr, "usage: %s <server-name> <server-port> <player-port> <observer-port>" % sys.argv[0]
	print >> sys.stderr, "\n"
	print >> sys.stderr, "This program waits for a connection on <observer-port> (for the observer) and"
	print >> sys.stderr, "on <player-port> (for the player). Then, it connectes to the server and"
	print >> sys.stderr, "passes messages of the server to both the player and the observer. All messages"
	print >> sys.stderr, "but end_turn messages, from the player, are sent to the server and messages from"
	print >> sys.stderr, "the observer are ignored. The end_turn message is sent to the server only if"
	print >> sys.stderr, "received by both the observer and the player."
	print >> sys.stderr, "\n"
	sys.exit(1)

server_name = sys.argv[1]
server_port = int(sys.argv[2])
player_port = int(sys.argv[3])
observer_port = int(sys.argv[4])

# Create sockets fot the observer and the player.
observer_srv = socket.socket()
observer_srv.bind(('localhost', observer_port))
observer_srv.listen(1)
player_srv = socket.socket()
player_srv.bind(('localhost', player_port))
player_srv.listen(1)

# Wait for the observer then the player.
observer, observer_addr = observer_srv.accept()
player, player_addr = player_srv.accept()

# Connect to the server.
server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
#server.settimeout(2)
try:
	server.connect((server_name, server_port))
except:
	print "unable to connect"
	sys.exit(1)

connections = [server, player, observer]
recv_length = 4096
end_turn_status = 0
end_turn_player = 0x1
end_turn_observer = 0x2

try:
	while 1:
		read_sockets, write_sockets, error_sockets = select.select(connections, [], [])
		for sock in read_sockets:
			if sock == player:
				data = player.recv(recv_length)
				#print "P>S   %s" % data
				# Le message end_turn est envoye au serveur uniquement si a la fois l'observeur et le joueur l'ont genere.
				if data == "end_turn\n":
					end_turn_status = end_turn_status | end_turn_player
					if end_turn_status == end_turn_observer | end_turn_player:
						end_turn_status = 0
						server.send(data)
				else:
					server.send(data)
			elif sock == observer:
				data = observer.recv(recv_length)
				#print "O>S %s" % data
				# Le message end_turn est envoye au serveur uniquement si a la fois l'observeur et le joueur l'ont genere.
				if data == "end_turn\n":
					end_turn_status = end_turn_status | end_turn_observer
					if end_turn_status == end_turn_observer | end_turn_player:
						end_turn_status = 0
						server.send(data)
			elif sock == server:
				data = server.recv(recv_length)
				#print "S>P&O %s" % data
				player.send(data)
				observer.send(data)
except Exception as e:
	print >> sys.stderr, "Error: %s" % str(e)

try:
	server.close()
except:
	pass

try:
	player.close()
except:
	pass

try:
	observer.close()
except:
	pass
