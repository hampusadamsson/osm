import threading
import queue
import sys
from threading import Event

"""

Class that receives all the messages from the server
"""

class StoppableThread(threading.Thread):

    def __init__(self,serverSocket):

        """

        Initiates the thread

        Key arguments:

        serverSocket - The socket to receive from
        """
        
        super(StoppableThread, self).__init__()
        self.serverSocket = serverSocket.makefile()
        self.messageQueue = queue.Queue()

    def run(self):

        """

        Runs the thread. Waits untill there is something to read and puts the data into the queue.
	If an exception occurs, "Disconnected" is put into the queue instead and the thread stops 
	receiving
        """
        
        while True:
            try:
                data = self.serverSocket.readline()
            except Exception as e:
                break
            if not data:
                break
            self.messageQueue.put(data)
        self.messageQueue.put("Disconnected")
        sys.exit(0)
        

    def returnQueue(self):

        """

        Returns the head of the queue

        Return: "empty" if the queue is empty, else the head of the list
        """
        
        if self.messageQueue.empty():
            return "empty"
        else:
            return self.messageQueue.get()
