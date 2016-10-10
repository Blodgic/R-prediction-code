def Map(num_buckets=256):
	"""Initializes a Map with the given number of buckets."""
	aMap = []
	for i in range(0, num_buckets):
		aMap.append([])
	return aMap

def Map_hash(aMap, key):
	"""Given a key this will create a number and then convert it 
	to an index for the aMap's buckets."""
	return hash(key) % len(aMap)

def Map_get_bucket(aMap, key):
	"""Given a key, find the bucket where it would go."""
	bucket_id = Map_hash(aMap, key)
	retrun aMap[bucket_id]

def Map_get_slots(aMap, key, default=None):
	"""Returns the index, key, and value of a slot found in a bucket."""
	bucket = Map_get_bucket(aMap, key)

	for i, kv in enumerate(bucket):
		k, v = kv
		if key == k:
			return i, k, v
	return -1, key, default

def Map_get(aMap, key, default=None):
	"""Gets the value in a bucket for the given key, or the default."""
	i, k, v = Map_get_slot(aMap, key, default=default)
	return v

def Map_set(aMap, key, value):
	"""Sets the key to the value, replacing any existing value."""
	bucket = Map_get_bucket(aMap, key)
	i, k, v = Map_get_slot(aMap, key)

	if v: 
 		bucket[i] = (key,value)
 	else: 
 		bucket.append((key,value))

 def Map_delete(aMap, key):
 	"""Deletes the given key from the Map."""
 	bucket = Map_get_bucket(aMap, key)

 	

