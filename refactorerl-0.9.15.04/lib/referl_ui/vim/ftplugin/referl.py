import subprocess, sys, vim, time, ast, re
from threading import Thread
import os

class CustomUndo:

	def monitorRefacSrv(self, stream):
		str = stream.readline()
		while str:
			self.handleRefChange(str)
			str = stream.readline()
	
	def monitorBufSrv(self, stream):
		str = stream.readline()
		while str:
			self.handleBufChange(str)
			str = stream.readline()

	def __init__(self, filename, path):
		self.startPlugin(filename, path)
	
	def startPlugin(self, filename, path):
		self.refpath = path
		self.undos = []
		self.redos = []
		self.buffers = dict([])
		self.reverts = []
		self.bufSrv = subprocess.Popen(vim.eval("s:ref_base_path") + "/bin/BufferServer", universal_newlines=True, stdin=subprocess.PIPE, stdout = subprocess.PIPE, shell=False, close_fds = True, bufsize=0)
	
		self.refacSrv = subprocess.Popen([vim.eval("s:ref_base_path") + "/bin/referl", "-base", vim.eval("s:ref_base_path"), "-db", vim.eval("s:ref_db_type"), "-dir", vim.eval("s:ref_datadir"), "-vim", "-" + vim.eval("s:ref_name_type"), vim.eval("s:ref_server_name")], universal_newlines=True, stdin=subprocess.PIPE, stdout = subprocess.PIPE, shell=False, bufsize=0)
	
		refMon = Thread(target=self.monitorRefacSrv, args=[self.refacSrv.stdout])
		refMon.daemon = True
		refMon.start()
	
		self.refCommands = list([])
	
		self.requests = dict([])
	
		self.renames = dict([])
		self.checkBuffer(filename)
	
	def stopPlugin(self):
		self.sendCommand("quit")
		self.sendRefCommand(["stop"])
		while self.bufSrv.poll():
			time.sleep(0.1)
		while self.refacSrv.poll():
			time.sleep(0.1)
	
	def checkBuffer(self, filename):
		if not filename in self.buffers.keys():
			self.sendRefCommand(["status", '"' + filename + '"'])
	
	def sendRefCommand(self, msg):
		self.refCommands.append(msg)
		self.refacSrv.stdin.write("{getid}.\n")
	
	def handleRefCommand(self, reqid):
		lastCommand = self.refCommands.pop()
		self.requests[reqid] = lastCommand
		command = "{{reqid,\"" + reqid  + "\"} " 
		for i in lastCommand:
			command += "," + i
		command += "}.\n"
		self.refacSrv.stdin.write(command)
	
	def handleCommandRes(self, reqid, res):
		if (self.requests[reqid][0]) == 'status':
			if (res[1][0][0] == 'present' and res[1][0][1] == 'true'):
				self.addBuffer(res[0])
				self.setBufferStatus(res[0], 'added')
			elif (res[1][0][0] == 'error'):
				self.addBuffer(res[0])
				self.setBufferStatus(res[0], 'error')
			self.loadMenu(res[0])
			
		del self.requests[reqid]
	
	def setBufferStatus(self, filename, status):
		self.buffers[filename]['status'] = status
	
	def addBuffer(self, filename):
		if not filename in self.buffers.keys():
			self.buffers[filename] = {'undo': [], 'redo': [], 'status': ''}
			self.sendCommand('open "' + filename + '"')
			self.readAnswer()
	
	def closeBuffer(self, filename):
		del self.buffers[filename]
		self.sendCommand('close "' + filename + '"')
	
	def sendCommand(self, msg):
		self.bufSrv.stdin.write(msg + "\n")
	
	def handleRefChange(self, msg):
		#f = open("./log2.txt", "a")
		#f.write(msg +"\n")
		#f.close()

		if not re.match('\(\'vim\',.*\)', msg.strip()):
			return
		
		try: 
			changes = ast.literal_eval(msg)
	
			for change in changes[1]:
				if not isinstance(change, tuple):
					return
				fst, snd = change[0], change[1]
				if fst == "modified":
					if snd in self.buffers.keys():
						self.sendCommand('load-opened "' + snd + '"')
					else:
						self.sendCommand('load-closed "' + snd + '"')
				elif fst == 'rename':
					if snd[1] in self.buffers.keys():
						self.sendCommand('rename-opened "' + snd[1] + '" "' + snd[0] + '"')
						oldData = self.buffers[snd[1]]
						self.buffers[snd[0]] = oldData
						del self.buffers[snd[1]]
						self.renames[snd[1]] = snd[0]
					else:
						self.sendCommand('rename-closed "' + snd[1] + '" "' + snd[0] + '"')
				elif fst == 'dropped':
					del self.buffers[snd]
					self.sendCommand('drop "' + snd + '"')
				elif fst == 'added':
					self.addBuffer(snd)
					self.setBufferStatus(snd, 'added')
				elif fst == 'error':
					self.addBuffer(snd)
					self.setBufferStatus(snd, 'error')
				elif fst == 'reqid':
					self.handleRefCommand(snd)
				elif len(change) >= 3 and change[1] == 'reply' and change[2][0] == 'ok':
					if isinstance(change[2][1], list):
						res = change[2][1][0]
						self.handleCommandRes(fst[1], res)
				elif len(change) >= 3 and change[1] == 'reply' and change[2][0] == 'error':
					if isinstance(change[2][1], list):
						res = change[2][1][0]
						self.handleCommandRes(fst[1], res)
					self.setBufferStatus(vim.eval('expand("%:p")'), 'error')
	
		except ValueError, Exception:
			return
	
	def handleBufChange(self, msg):
		try: 
			lists = msg.split(';')
	
			if len(lists) < 3:
				return
	
			revert = ast.literal_eval(lists[0])
			undo = ast.literal_eval('{' + lists[1].replace('|', ',') + '}')
			redo = ast.literal_eval('{' + lists[2].replace('|', ',') + '}')
			openedRename = {}
			closedRename = {}
			if len(lists) >= 4:
				openedRename = ast.literal_eval('{' + lists[3].replace('->', ':') + '}')
			if len(lists) >= 5:
				closedRename = ast.literal_eval('{' + lists[4].replace('->', ':') + '}')
			
			
			for fn, cs in undo.items():
				self.buffers[fn]['undo'] = cs
	
			for fn, cs in redo.items():
				self.buffers[fn]['redo'] = cs
	
	
			self.handleOpenedRenames(openedRename)
			self.handleClosedRenames(closedRename)
			self.handleReverts(revert)
					
		except ValueError:
			return
	
	def handleReverts(self, reverts):
		#act = vim.eval("expand('%:p')")
		for fn in reverts:
			self.sendRefCommand(['add', '"' + fn + '"'])
			vim.command(":edit! " + fn)
		#vim.command(":edit " + act)

	
	def handleOpenedRenames(self, renames):
		#f = open("./log2.txt", "a")
		#f.write(str(renames) +"\n")
		act = vim.eval("expand('%:p')")
		#f.write(act + "\n")
		#f.close()
		if act in renames.keys():
			newFilename = renames[act]
			oldData = self.buffers[act]
			self.buffers[newFilename] = oldData
			del self.buffers[act]
			os.rename(act, newFilename)
			vim.command(":edit! " + newFilename)
			self.sendRefCommand(['add', '"' + newFilename + '"'])
			self.sendRefCommand(['drop', '"' + act + '"'])
			vim.command("bw! " + act)
			del renames[act]
	
		for ofn, nfn in renames:
			oldData = self.buffers[ofn]
			self.buffers[nfn] = oldData
			del self.buffers[ofn]
			self.sendRefCommand(['drop', '"' + ofn + '"'])
			os.rename(ofn, nfn)
			self.sendRefCommand(['add', '"' + nfn + '"'])
			vim.command(":badd " + nfn)
			vim.command("bw! " + ofn)

	def handleClosedRenames(self, renames):
		for ofn, nfn in renames:
			self.sendRefCommand(['drop', '"' + ofn + '"'])
			self.sendRefCommand(['add', '"' + nfn + '"'])

	
	def readAnswer(self):
		str = self.bufSrv.stdout.readline()
		self.handleBufChange(str)
	
	def loadMenu(self, filename):
		#f = open("./log2.txt", "a")
		#f.write(str(self.buffers) +"\n")
		#f.close()
	
		if not filename in self.buffers.keys():
			vim.command(":menu disable RefactorErl.Undo")
			vim.command(":menu disable RefactorErl.Redo")
			vim.command(":menu disable RefactorErl.Function")
			vim.command(":menu disable RefactorErl.Introduce/Eliminate")
			vim.command(":menu disable RefactorErl.Move\ to\ another\ module")
			vim.command(":menu disable RefactorErl.Renaming")
			return
		else:
			if self.buffers[filename]['undo'] == []:
				vim.command(":menu disable RefactorErl.Undo")
			else:
				vim.command(":menu enable RefactorErl.Undo")
	
			if self.buffers[filename]['redo'] == []:
				vim.command(":menu disable RefactorErl.Redo")
			else:
				vim.command(":menu enable RefactorErl.Redo")
	
			vim.command(":menu enable RefactorErl.Function")
			vim.command(":menu enable RefactorErl.Introduce/Eliminate")
			vim.command(":menu enable RefactorErl.Move\ to\ another\ module")
			vim.command(":menu enable RefactorErl.Renaming")
	
		for m in self.undos:
			vim.command("unmenu RefactorErl.Undo."+ m)
	
		menuData = self.buffers[filename]
		self.undos = menuData['undo']
		i = 1
		for m in menuData['undo']:
			vim.command("noremenu  <silent> &RefactorErl.&Undo."+ m + " :call <SID>customUndo(" + str(i) + ")<CR>")
			i+=1
		
		for m in self.redos:
			vim.command("unmenu RefactorErl.Redo."+ m)
		
		i = 1
		self.redos = menuData['redo']
		for m in menuData['redo']:
			vim.command("noremenu  <silent> &RefactorErl.&Redo."+ m + " :call <SID>customRedo(" + str(i) + ")<CR>")
			i+=1
	
