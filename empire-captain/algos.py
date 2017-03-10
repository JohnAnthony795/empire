import heapq

directions = [ (+1,  0), (+1, -1), ( 0, -1), (-1,  0), (-1, +1), ( 0, +1) ]

def cube_ring(center, radius):
	if radius == 0:
		return [center]
	q, r = center
	results = []
	for i in range(len(directions)):
		qa, ra = directions[i]
		qb, rb = directions[(i + 1) % len(directions)]
		qd, rd = qb - qa, rb - ra
		if qd != 0:
			qd = qd / abs(qd)
		if rd != 0:
			rd = rd / abs(rd)
		for j in range(radius):
			results.append((q + radius * qa + j * qd, r + radius * ra + j * rd))
	return results

def add_locations(location_a, location_b):
	return location_b[0] + location_a[0], location_b[1] + location_a[1]

def are_locations_adjacents(location_a, location_b):
	delta = location_b[0] - location_a[0], location_b[1] - location_a[1]
	return delta in directions

def delta_to_direction(location_a, location_b):
	delta = location_b[0] - location_a[0], location_b[1] - location_a[1]
	return directions.index(delta) 

def get_tiles_distance(location_a, location_b):
	qa, ra = location_a
	qb, rb = location_b
	return (abs (qa - qb) + abs (qa + ra - qb - rb) + abs (ra - rb)) / 2

def neighbors_helper(location, criteria):
	neighbors = [add_locations(location, x) for x in directions]
	return [x for x in neighbors if criteria(x)]

def neighbors_terrains(situation, location, terrains):
	criteria = lambda x: situation.is_in_map(x) and situation.get_tile(x).terrain in terrains
	return neighbors_helper(location, criteria)

def neighbors_piece(situation, location, piece):
	criteria = lambda x: situation.can_player_piece_be_on(piece, x)
	return neighbors_helper(location, criteria)

def reconstitue_path(came_from, start, goal):
	if goal in came_from:
		l = [goal]
		x = goal
		while x != start:
			x = came_from[x]
			l = [x] + l
		return l
	return None

# TODO: start == piece.get_location()
def astar_goal(start, goal, situation, piece):
	heuristic = get_tiles_distance
	frontier = []
	heapq.heappush(frontier, (start, 0))
	came_from = {start: None}
	cost_so_far = {start: 0}
	while len(frontier) > 0:
		current, tmp = heapq.heappop(frontier)
		if current == goal:
			break
		if current == start or situation.can_player_piece_safely_be_on(piece, current):
			nexts = neighbors_piece(situation, current, piece)
			for next in nexts:
				new_cost = cost_so_far[current] + 1
				if next not in cost_so_far or new_cost < cost_so_far[next]:
					cost_so_far[next] = new_cost
					priority = new_cost + heuristic(goal, next)
					heapq.heappush(frontier, (next, priority))
					came_from[next] = current
	return reconstitue_path(came_from, start, goal)

def breadth_first_helper(start, depth, neighbors, criteria, can_be_intermediate):
	frontier = [(0, start)]
	visited = {start: True}
	came_from = {start: None}
	path_len = {start: 0}
	reachable = []
	while len(frontier) > 0:
		current_depth, current = frontier[0]
		if criteria is not None and criteria(current):
			return reconstitue_path(came_from, start, current)
		del frontier[0]
		if (current == start or can_be_intermediate(current)) and current_depth < depth:
			for next in neighbors(current):
				if next not in visited:
					reachable.append(next)
					frontier.append((current_depth + 1, next))
					visited[next] = True
					came_from[next] = current
					path_len[next] = current_depth + 1
	if criteria is None:
		return reachable, came_from, path_len
	return None

def breadth_first_piece_search(start, depth, situation, piece, criteria):
	neighbors = lambda x: neighbors_piece(situation, x, piece)
	can_be_intermediate = lambda x: situation.can_player_piece_safely_be_on(piece, x)
	return breadth_first_helper(start, depth, neighbors, criteria, can_be_intermediate)

def breadth_first_piece(start, depth, situation, piece):
	criteria = lambda x: situation.can_player_piece_safely_be_on(piece, x)
	neighbors = lambda x: neighbors_helper(x, criteria)
	can_be_intermediate = lambda x: situation.can_player_piece_safely_be_on(piece, x)
	return breadth_first_helper(start, depth, neighbors, None, can_be_intermediate)

def breadth_first_terrains(start, depth, situation, terrains):
	neighbors = lambda x: neighbors_terrains(situation, x, terrains)
	can_be_intermediate = lambda x: True
	return breadth_first_helper(start, depth, neighbors, None, can_be_intermediate)
