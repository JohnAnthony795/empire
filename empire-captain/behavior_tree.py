import inspect
import random

import algos

do_debug = False

def call(name):
	stack = inspect.stack()
	fun = stack[1][3]
	print "%s> %s.%s" % (" " * len(stack), name, fun)

def ret(value):
	stack = inspect.stack()
	print "%s = %d" % (" " * len(stack), value)

class Routine:

	SUCCESS = 0
	FAILURE = 1
	RUNNING = 2

	def __init__(self, info):
		self.info = info

	def act(self):
		assert False, "not implemented"

	def bool2state(self, b):
		return self.SUCCESS if b else self.FAILURE

class Selector(Routine):

	def __init__(self, info, routines):
		Routine.__init__(self, info)
		assert len(routines) > 0
		self.routines = routines

	def act(self):
		if do_debug: call(self.__class__.__name__)
		for routine in self.routines:
			state = routine.act()
			if do_debug: ret(state)
			if state != self.FAILURE:
				return state
		return self.FAILURE

class SelectorRandom(Selector):

	def __init__(self, info, routines):
		Selector.__init__(self, info)
		self._routines = routines

	def act(self):
		if do_debug: call(self.__class__.__name__)
		self.routines = self._routines[:]
		random.shuffle(self.routines)
		return Selector.act(self)

class SelectorMemorize(Routine):

	def __init__(self, info, routines):
		Routine.__init__(self, info)
		assert len(routines) > 0
		self.routines = routines
		self.position = None

	def act(self):
		if do_debug: call(self.__class__.__name__)
		if self.position is None:
			self.position = 0
		while self.position < len(self.routines):
			state = self.routines[self.position].act()
			if do_debug: ret(state)
			if state == self.SUCCESS:
				self.position = None
				return state
			elif state == self.FAILURE:
				self.position = self.position + 1
			else:
				return state
		self.position = None
		return self.FAILURE

class SequenceMemorize(Routine):

	def __init__(self, info, routines):
		Routine.__init__(self, info)
		assert len(routines) > 0
		self.routines = routines
		self.position = None

	def act(self):
		if do_debug: call(self.__class__.__name__)
		if self.position is None:
			self.position = 0
		while self.position < len(self.routines):
			state = self.routines[self.position].act()
			if do_debug: ret(state)
			if state == self.SUCCESS:
				self.position = self.position + 1
			if state == self.FAILURE:
				self.position = None
			if state != self.SUCCESS:
				return state
		self.position = None
		return self.SUCCESS

class Sequence(Routine):

	def __init__(self, info, routines):
		Routine.__init__(self, info)
		assert len(routines) > 0
		self.routines = routines

	def act(self):
		if do_debug: call(self.__class__.__name__)
		for routine in self.routines:
			state = routine.act()
			if do_debug: ret(state)
			if state != self.SUCCESS:
				return state
		return self.SUCCESS

class Success(Routine):

	def act(self):
		if do_debug: call(self.__class__.__name__)
		return self.SUCCESS

class Running(Routine):

	def act(self):
		if do_debug: call(self.__class__.__name__)
		return self.RUNNING

class Decorator(Routine):

	def __init__(self, info, routine):
		Routine.__init__(self, info)
		self.routine = routine

class UntilFail(Decorator):

	def act(self):
		if do_debug: call(self.__class__.__name__)
		while True:
			state = self.routine.act()
			if do_debug: ret(state)
			if state == self.FAILURE:
				return self.SUCCESS
			elif state != self.SUCCESS:
				return state

class Inverser(Decorator):

	def __init__(self, info, routine):
		Routine.__init__(self, info)
		self.routine = routine

	def act(self):
		if do_debug: call(self.__class__.__name__)
		state = self.routine.act()
		if do_debug: ret(state)
		if state == self.SUCCESS:
			return self.FAILURE
		elif state == self.FAILURE:
			return self.SUCCESS
		return state

class WhileSuccess(Decorator):

	def act(self):
		if do_debug: call(self.__class__.__name__)
		while True:
			state = self.routine.act()
			if do_debug: ret(state)
			if state != self.SUCCESS:
				return state

class WhileRunning(Decorator):

	def act(self):
		if do_debug: call(self.__class__.__name__)
		while True:
			state = self.routine.act()
			if do_debug: ret(state)
			if state != self.RUNNING:
				return state

# TODO: untested
class GetPlayerPiecesStack(Routine):

	def __init__(self, info, stack_name):
		Routine.__init__(self, info)
		self.stack_name = stack_name

	def act(self):
		data = self.info.data
		situation = self.info.situation
		data["stacks"][self.stack_name] = situation.get_player_pieces()
		return self.SUCCESS

# TODO: untested
class PopPlayerPiece(Routine):

	def __init__(self, info, stack_name, piece_ref):
		Routine.__init__(self, info)
		self.stack_name = stack_name
		self.piece_ref = piece_ref

	def act(self):
		data = self.info.data
		if self.stack_name not in data:
			return self.FAILURE
		stack = data["stacks"][self.stack_name]
		if len(stack) == 0:
			return self.FAILURE
		data[self.piece_ref] = stack.pop()
		return self.SUCCESS

class PieceRoutine(Routine):

	def act(self):
		if do_debug: call(self.__class__.__name__)
		if not self.info.situation.is_player_piece(self.info.piece.piece_id):
			return self.FAILURE
		return self.act_piece()

# TODO: IsStepValid
class IsPathValid(PieceRoutine):

	def act_piece(self):
		situation = self.info.situation
		piece = self.info.piece
		path = piece.path
		location = piece.get_location()
		if path is None or len(path) == 0:
			return self.FAILURE
		for i in range(len(path) - 1):
			if not (algos.are_locations_adjacents(location, path[i]) and situation.can_player_piece_safely_be_on(piece, path[i])):
				return self.FAILURE
			location = path[i]
		destination = path[-1]
		if not (algos.are_locations_adjacents(location, destination) and situation.can_player_piece_be_on(piece, destination)):
			return self.FAILURE
		return self.SUCCESS

class FindPath(PieceRoutine):

	def act_piece(self):
		piece = self.info.piece
		situation = self.info.situation
		piece.path = algos.astar_goal(piece.get_location(), piece.destination, situation, piece)
		if piece.path is None:
			return self.FAILURE
		del piece.path[0]
		return self.SUCCESS

class FindPathToGroup(PieceRoutine):

	def act_piece(self):
		piece = self.info.piece
		situation = self.info.situation
		continent = self.info.continent
		group = continent.get_group(self.info.piece.target)
		def criteria(a):
			locations = [algos.add_locations(a, b) for b in algos.directions]
			tests = [situation.is_in_map(b) and continent.get_group(b) == group for b in locations]
			return any(tests)
		depth = situation.width * situation.height
		piece.path = algos.breadth_first_piece_search(piece.get_location(), depth, situation, piece, criteria)
		if piece.path is None:
			return self.FAILURE
		del piece.path[0]
		return self.SUCCESS

class StepTo(PieceRoutine):

	def act_piece(self):
		piece = self.info.piece
		situation = self.info.situation
		path = piece.path
		# Check the next destination (not the full path).
		if path is None or len(path) == 0:
			return self.FAILURE
		destination = path[0]
		if (not situation.can_player_piece_safely_be_on(piece, destination) and len(path) > 1) or \
				(not situation.can_player_piece_be_on(piece, destination) and len(path) == 1):
			return self.FAILURE
		if piece.steps_left == 0:
			return self.FAILURE
		try:
			location = piece.get_location()
			communication = self.info.communication
			piece.steps_left -= 1
			direction = algos.delta_to_direction(location, destination)
			communication.action("move %d %d" % (piece.piece_id, direction))
			del path[0]
			return self.SUCCESS
		except:
			piece.path = None
			return self.FAILURE

class ArrivedToGroup(PieceRoutine):

	def act_piece(self):
		location = self.info.piece.get_location()
		continent = self.info.continent
		situation = self.info.situation
		group = continent.get_group(self.info.piece.target)
		locations = [algos.add_locations(location, b) for b in algos.directions]
		return self.bool2state(any([situation.is_in_map(b) and continent.get_group(b) == group for b in locations]))

class Arrived(PieceRoutine):

	def act_piece(self):
		return self.bool2state(self.info.piece.get_location() == self.info.piece.destination)

# The piece scout first unexplored tile then non-visible tile.
class ScoutChooseDestination(PieceRoutine):

	def act_piece(self):
		self.info.piece.destination = None
		situation = self.info.situation
		piece = self.info.piece
		location = piece.get_location()
		depth = situation.width * situation.height
		result = algos.breadth_first_piece(location, depth, situation, piece)
		destinations = result[0]
		destinations = [x for x in destinations if situation.get_terrain(x) is None]
		if len(destinations) == 0:
			destinations = result[0]
			destinations = [x for x in destinations if not situation.get_tile(x).visible]
		if len(destinations) == 0:
			return self.FAILURE
		piece.destination = random.choice(destinations)
		return self.SUCCESS

class RandomWalk(PieceRoutine):

	def act_piece(self):
		self.info.piece.destination = None
		situation = self.info.situation
		piece = self.info.piece
		location = piece.get_location()
		depth = situation.piece_types[piece.piece_type_id].speed * 2
		result = algos.breadth_first_piece(location, depth, situation, piece)
		destinations = result[0]
		if len(destinations) == 0:
			return self.FAILURE
		piece.destination = random.choice(destinations)
		return self.SUCCESS

class PieceExists(PieceRoutine):

	def act_piece(self):
		return self.bool2state(self.info.situation.is_player_piece(self.info.piece.piece_id))

class Empty(Decorator):

	def act(self):
		if do_debug: call(self.__class__.__name__)
		piece = self.info.piece
		if len(self.info.piece.transport) == 0:
			return self.routine.act()
		return self.FAILURE

class NotFull(Decorator):

	def act(self):
		if do_debug: call(self.__class__.__name__)
		piece = self.info.piece
		piece_type = self.info.situation.piece_types[piece.piece_type_id]
		if len(self.info.piece.transport) < piece_type.capacity:
			return self.routine.act()
		return self.FAILURE

class Full(Decorator):

	def act(self):
		if do_debug: call(self.__class__.__name__)
		piece = self.info.piece
		piece_type = self.info.situation.piece_types[piece.piece_type_id]
		if len(self.info.piece.transport) == piece_type.capacity:
			return self.routine.act()
		return self.FAILURE

class InCity(Decorator):

	def act(self):
		if do_debug: call(self.__class__.__name__)
		if not self.info.situation.is_player_piece(self.info.piece.piece_id):
			return self.FAILURE
		if self.info.piece.parent.is_city():
			return self.routine.act()
		return self.FAILURE

class StepsLeft(PieceRoutine):

	def act_piece(self):
		return self.bool2state(self.info.piece.steps_left != 0)

class CanInvade(PieceRoutine):

	def act_piece(self):
		piece = self.info.piece
		piece_type = self.info.situation.piece_types[piece.piece_type_id]
		return self.bool2state(piece_type.can_invade)

class SelectReachableContinentWithNobody(PieceRoutine):

	def act_piece(self):
		location = self.info.piece.get_location()
		group = self.info.continent.get_group(location)
		stats = self.info.stats
		continent = self.info.continent
		if group not in stats.group_stats:
			return self.FAILURE
		candidates = []
		for link in self.info.continent.get_neighbors(group):
			if link in stats.group_stats and (stats.group_stats[link]["ep"] + stats.group_stats[link]["ec"] == 0) and \
					(stats.group_stats[link]["pp"] + stats.group_stats[link]["pc"] == 0):
				candidates.append(link)
		if len(candidates) == 0:
			return self.FAILURE
		# We memorize a location in the group. It is not necessarly the destination, but the identifier of the
		# group may disapear due to merging!
		self.info.piece.target = continent.get_locations(random.choice(candidates))[0]
		return self.SUCCESS

class Take(PieceRoutine):

	def act_piece(self):
		done_all_group = False
		continent = self.info.continent
		situation = self.info.situation
		piece = self.info.piece
		piece_type = situation.piece_types[piece.piece_type_id]
		available = piece_type.capacity - len(piece.transport)
		if available == 0:
			return self.SUCCESS
		result = []
		radius = 1
		group = self.info.continent.get_group(piece.target)
		#print "TAKE: grp=", group
		while not done_all_group and len(result) < available:
			locations = algos.cube_ring(piece.get_location(), radius)
			done_all_group = True # We assume that we sweep the whole group.
			#print "TAKE: loop=", len(locations)
			for location in locations:
				#if situation.is_in_map(location):
					#print "TAKE: location_grp=", continent.get_group(location), situation.get_content(location)
				if situation.is_in_map(location) and continent.get_group(location) == group:
					done_all_group = False
					content = situation.get_content(location)
					# Take an available piece or a piece already affected to this transport.
					if content is not None and \
							content.is_piece() and \
							content.owner == situation.player_id and \
							content.piece_type_id in piece_type.transportable:
						#print "TAKE: check=", content.role, content.piece_type_id, content.piece_id
						if content.role == "moveto_transport" and content.destination == piece.get_location() or content.role != "moveto_transport":
							#print "TAKE: add"
							result.append(content)
					# Check also pieces in cities.
					if content is not None and \
							content.is_owned_city() and \
							content.owner == situation.player_id:
						#print "TAKE: owned city", len(content.content)
						for transported in content.content:
							#print "TAKE: check=", transported.role, transported.piece_type_id, transported.piece_id
							if transported.piece_type_id in piece_type.transportable:
								if transported.role == "moveto_transport" and transported.destination == piece.get_location() or transported.role != "moveto_transport":
									#print "TAKE: add"
									result.append(transported)
			radius = radius + 1
		#print "TAKE: ", len(result), available
		if len(result) > available:
			result = result[:available]
		for transported in result:
			# TODO: set a variable in the piece to indicate that it must moveto the transport. This indication
			# must be tested in its behavior. This way, we keep intact its behavior.
			transported.destination = piece.get_location()
			transported_info = Info(self.info.situation, self.info.communication, self.info.continent, self.info.stats, self.info.influence, self.info.parameters, self.info.factory)
			transported_info.piece = transported
			transported.behavior = self.info.factory.create(transported_info, "moveto_transport", self.info.parameters.behaviors)
			transported.role = "moveto_transport"
		return self.RUNNING

class Reset(PieceRoutine):

	def act_piece(self):
		piece = self.info.piece
		piece.destination = None
		piece.behavior = None
		piece.role = ""
		return self.SUCCESS

class ReachTransport(PieceRoutine):

	def act_piece(self):
		piece = self.info.piece
		piece.destination = None
		piece.behavior = None
		piece.role = ""
		# TODO: set a variable in the piece to indicate that it must moveto the transport. This indication
		# must be tested in its behavior. This way, we keep intact its behavior.
		piece.behavior = self.info.factory.create(self.info, "wait", self.info.parameters.behaviors)
		piece.role = "wait"
		return self.SUCCESS

class Land(PieceRoutine):

	def act_piece(self):
		if len(self.info.piece.transport) == 0:
			return self.SUCCESS
		for transported in self.info.piece.transport:
			piece = self.info.situation.player_pieces[transported]
			piece.behavior = None
			piece.role = ""
		return self.RUNNING

class SelectReachableContinentWithPlayerAndEnemy(PieceRoutine):

	def act_piece(self):
		if do_debug: call(self.__class__.__name__)
		location = self.info.piece.get_location()
		group = self.info.continent.get_group(location)
		stats = self.info.stats
		continent = self.info.continent
		if group not in stats.group_stats:
			return self.FAILURE
		candidates = []
		for link in self.info.continent.get_neighbors(group):
			if link in stats.group_stats and (stats.group_stats[link]["ep"] + stats.group_stats[link]["ec"] > 0) and \
					(stats.group_stats[link]["pp"] + stats.group_stats[link]["pc"] > 0):
				candidates.append(link)
		if len(candidates) == 0:
			return self.FAILURE
		# We memorize a location in the group. It is not necessarly the destination, but the identifier of the
		# group may disapear due to merging!
		self.info.piece.target = continent.get_locations(random.choice(candidates))[0]
		return self.SUCCESS

class SelectReachableSafeContinentWithLotOfPlayer(PieceRoutine):

	def act_piece(self):
		if do_debug: call(self.__class__.__name__)
		location = self.info.piece.get_location()
		group = self.info.continent.get_group(location)
		stats = self.info.stats
		continent = self.info.continent
		if group not in stats.group_stats:
			return self.FAILURE
		candidate = None
		candidate_stat = None
		for link in self.info.continent.get_neighbors(group):
			if link in stats.group_stats and (stats.group_stats[link]["ep"] + stats.group_stats[link]["ec"] == 0):
				if candidate is None or stats.group_stats[link]["pp"] + stats.group_stats[link]["pc"] > candidate_stat:
					candidate_stat = stats.group_stats[link]["pp"] + stats.group_stats[link]["pc"]
					candidate = link
		if candidate is None:
			return self.FAILURE
		# We memorize a location in the group. It is not necessarly the destination, but the identifier of the
		# group may disapear due to merging!
		self.info.piece.target = continent.get_locations(candidate)[0]
		return self.SUCCESS

class SelectReachableContinentWithEnemy(PieceRoutine):

	def act_piece(self):
		if do_debug: call(self.__class__.__name__)
		location = self.info.piece.get_location()
		group = self.info.continent.get_group(location)
		stats = self.info.stats
		continent = self.info.continent
		if group not in stats.group_stats:
			return self.FAILURE
		candidates = []
		for link in self.info.continent.get_neighbors(group):
			if link in stats.group_stats and (stats.group_stats[link]["ep"] + stats.group_stats[link]["ec"] > 0):
				candidates.append(link)
		if len(candidates) == 0:
			return self.FAILURE
		# We memorize a location in the group. It is not necessarly the destination, but the identifier of the
		# group may disapear due to merging!
		self.info.piece.target = continent.get_locations(random.choice(candidates))[0]
		return self.SUCCESS

class ContinentContainsEnemy(PieceRoutine):

	def act_piece(self):
		if do_debug: call(self.__class__.__name__)
		location = self.info.piece.get_location()
		group = self.info.continent.get_group(location)
		stats = self.info.stats
		return self.bool2state(group in stats.group_stats and (stats.group_stats[group]["ep"] > 0 or stats.group_stats[group]["ec"] > 0))

class ContinentContainsFreeCities(PieceRoutine):

	def act_piece(self):
		if do_debug: call(self.__class__.__name__)
		location = self.info.piece.get_location()
		group = self.info.continent.get_group(location)
		stats = self.info.stats
		return self.bool2state(group in stats.group_stats and stats.group_stats[group]["fc"] > 0)

class SelectNearestEnemyPiece(PieceRoutine):

	def act_piece(self):
		if do_debug: call(self.__class__.__name__)
		situation = self.info.situation
		piece = self.info.piece
		location = piece.get_location()
		depth = situation.piece_types[piece.piece_type_id].speed
		criteria = lambda x: situation.is_tile_enemy_piece(x)
		result = algos.breadth_first_piece_search(location, depth, situation, piece, criteria)
		if result is None:
			return self.FAILURE
		else:
			piece.path = result
			del piece.path[0]
			piece.destination = result[-1]
			return self.SUCCESS

class MoveTowardFreeCities(PieceRoutine):

	def act_piece(self):
		if do_debug: call(self.__class__.__name__)
		situation = self.info.situation
		piece = self.info.piece
		location = piece.get_location()
		depth = situation.piece_types[piece.piece_type_id].speed
		result = algos.breadth_first_piece(location, depth, situation, piece)
		destinations = result[0]
		if len(destinations) == 0:
			return self.FAILURE
		else:
			influence = self.info.influence
			scores = [influence.influence_free_cities[q][r] for (q, r) in destinations]
			index_max = max(list(range(len(scores))), key=lambda x: scores[x])
			piece.destination = destinations[index_max]
			piece.path = None
			return self.SUCCESS

class MoveTowardEnemy(PieceRoutine):

	def act_piece(self):
		if do_debug: call(self.__class__.__name__)
		situation = self.info.situation
		piece = self.info.piece
		location = piece.get_location()
		depth = situation.piece_types[piece.piece_type_id].speed
		result = algos.breadth_first_piece(location, depth, situation, piece)
		destinations = result[0]
		if len(destinations) == 0:
			return self.FAILURE
		else:
			influence = self.info.influence
			scores = [influence.influence_enemies_cities[q][r] + influence.influence_enemies_pieces[q][r] for (q, r) in destinations]
			index_max = max(list(range(len(scores))), key=lambda x: scores[x])
			piece.destination = destinations[index_max]
			piece.path = None
			return self.SUCCESS

class SelectNearestPlayerCity(PieceRoutine):

	def act_piece(self):
		if do_debug: call(self.__class__.__name__)
		situation = self.info.situation
		piece = self.info.piece
		location = piece.get_location()
		depth = situation.piece_types[piece.piece_type_id].speed
		criteria = lambda x: situation.is_tile_player_city(x)
		result = algos.breadth_first_piece_search(location, depth, situation, piece, criteria)
		if result is None:
			return self.FAILURE
		else:
			piece.path = result
			piece.destination = result[-1]
			del piece.path[0]
			return self.SUCCESS

class SelectNearestEnemyCity(PieceRoutine):

	def act_piece(self):
		if do_debug: call(self.__class__.__name__)
		situation = self.info.situation
		piece = self.info.piece
		location = piece.get_location()
		depth = situation.piece_types[piece.piece_type_id].speed
		criteria = lambda x: situation.is_tile_enemy_city(x)
		result = algos.breadth_first_piece_search(location, depth, situation, piece, criteria)
		if result is None:
			return self.FAILURE
		else:
			piece.path = result
			piece.destination = result[-1]
			del piece.path[0]
			return self.SUCCESS

class SelectFreeCity(PieceRoutine):

	def act_piece(self):
		if do_debug: call(self.__class__.__name__)
		situation = self.info.situation
		piece = self.info.piece
		location = piece.get_location()
		depth = situation.piece_types[piece.piece_type_id].speed * 10
		criteria = lambda x: situation.is_tile_free_city(x)
		result = algos.breadth_first_piece_search(location, depth, situation, piece, criteria)
		if result is None:
			return self.FAILURE
		else:
			piece.path = result
			del piece.path[0]
			piece.destination = result[-1]
			return self.SUCCESS

class Info:

	def __init__(self, situation, communication, continent, stats, influence, parameters, factory):
		self.piece = None
		self.situation = situation
		self.communication = communication
		self.continent = continent
		self.stats = stats
		self.influence = influence
		self.parameters = parameters
		self.factory = factory

class Factory:

	def create(self, info, name, behaviors):
		def compile_behavior(behavior):
			#print ":", behavior
			if behavior[0] == "inverser":
				return Inverser(info, compile_behavior(behavior[1]))
			if behavior[0] == "running":
				return Running(info)
			if behavior[0] == "success":
				return Success(info)
			if behavior[0] == "while_success":
				return WhileSuccess(info, compile_behavior(behavior[1]))
			if behavior[0] == "while_running":
				return WhileRunning(info, compile_behavior(behavior[1]))
			if behavior[0] == "until_fail":
				return UntilFail(info, compile_behavior(behavior[1]))
			if behavior[0] == "selector":
				return Selector(info, [compile_behavior(x) for x in behavior[1]])
			if behavior[0] == "selector_random":
				return SelectorRandom(info, [compile_behavior(x) for x in behavior[1]])
			if behavior[0] == "selector_memorize":
				return SelectorMemorize(info, [compile_behavior(x) for x in behavior[1]])
			if behavior[0] == "sequence":
				return Sequence(info, [compile_behavior(x) for x in behavior[1]])
			if behavior[0] == "sequence_memorize":
				return SequenceMemorize(info, [compile_behavior(x) for x in behavior[1]])

			if behavior[0] == "?is_path_valid":
				return IsPathValid(info)
			if behavior[0] == "find_path":
				return FindPath(info)
			if behavior[0] == "find_path_to_group":
				return FindPathToGroup(info)
			if behavior[0] == "stepto":
				return StepTo(info)

			if behavior[0] == "?arrived_to_group":
				return ArrivedToGroup(info)
			if behavior[0] == "?arrived":
				return Arrived(info)

			if behavior[0] == "move_toward_enemy":
				return MoveTowardEnemy(info)
			if behavior[0] == "move_toward_free_cities":
				return MoveTowardFreeCities(info)
			if behavior[0] == "select_nearest_enemy_piece":
				return SelectNearestEnemyPiece(info)
			if behavior[0] == "select_nearest_player_city":
				return SelectNearestPlayerCity(info)
			if behavior[0] == "select_nearest_enemy_city":
				return SelectNearestEnemyCity(info)
			if behavior[0] == "select_free_city":
				return SelectFreeCity(info)
			if behavior[0] == "?continent_contains_enemy":
				return ContinentContainsEnemy(info)
			if behavior[0] == "?continent_contains_free_cities":
				return ContinentContainsFreeCities(info)
			if behavior[0] == "?can_invade":
				return CanInvade(info)
			if behavior[0] == "?steps_left":
				return StepsLeft(info)
			if behavior[0] == "link":
				return compile_behavior(behaviors[behavior[1]])
			if behavior[0] == "random_walk":
				return RandomWalk(info)
			if behavior[0] == "?not_full":
				return NotFull(info, compile_behavior(behavior[1]))
			if behavior[0] == "?full":
				return Full(info, compile_behavior(behavior[1]))
			if behavior[0] == "?empty":
				return Empty(info, compile_behavior(behavior[1]))
			if behavior[0] == "?in_city":
				return InCity(info, compile_behavior(behavior[1]))
			if behavior[0] == "?piece_exists":
				return PieceExists(info)
			if behavior[0] == "scout_choose_destination":
				return ScoutChooseDestination(info)
			if behavior[0] == "select_reachable_continent_with_player_and_enemy":
				return SelectReachableContinentWithPlayerAndEnemy(info)
			if behavior[0] == "select_reachable_continent_with_nobody":
				return SelectReachableContinentWithNobody(info)
			if behavior[0] == "select_reachable_continent_with_enemy":
				return SelectReachableContinentWithEnemy(info)
			if behavior[0] == "select_reachable_safe_continent_with_lot_of_player":
				return SelectReachableSafeContinentWithLotOfPlayer(info)
			if behavior[0] == "land":
				return Land(info)
			if behavior[0] == "take":
				return Take(info)
			if behavior[0] == "reach_transport":
				return ReachTransport(info)
			if behavior[0] == "reset":
				return Reset(info)
			assert False, "unknown behavior:" + str(behavior)
		#print behaviors[name]
		return compile_behavior(behaviors[name])
