import subprocess

def query(hashtag):
	pipe = subprocess.PIPE
	p = subprocess.Popen(['./main.erl', hashtag], stdout=pipe, stderr=pipe)
	out, err = p.communicate()
	result = clean_output(str(out))
	return float(result)

def clean_output(output):
	bad_characters = "`~!@$%^&*()-_=+[{]}\|;:,<>/?abcdefghijklmnopqrstuvwxyz"
	bad_characters += '"'
	bad_characters += "'"
	for char in bad_characters:
		output = output.replace(char, '')
	return output