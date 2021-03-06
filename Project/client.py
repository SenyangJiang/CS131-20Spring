import asyncio
import time
import json

class ClientMessage:
    def __init__(self, client_id="kiwi.cs.ucla.edu", coordinates="+34.068930-118.445127"):
        self.client_id = client_id
        self.coordinates = coordinates

    def text(self, command_name, **kwargs):
        command_table = {
            'IAMAT': self.i_am_at(),
            'WHATSAT': self.whats_at(**kwargs)
        }
        return command_table.get(command_name, "")

    def i_am_at(self):
        """
        reporting where am I at
        """
        return f"IAMAT {self.client_id} {self.coordinates} {time.time()}"

    def whats_at(self, another_client=None, radius=0, max_results=0):
        """
        get at most max_results places within the radius centered around another client
        those locations are expected to be given by Google Map
        the unit of radius is kilometer
        """
        if another_client is None:
            another_client = self.client_id
        return f"WHATSAT {another_client} {radius} {max_results}"

class Client:
    def __init__(self, ip='127.0.0.1', port=11670, name='client', message_max_length=1e6):
        """
        127.0.0.1 is the localhost
        port could be any port
        """
        self.ip = ip
        self.port = port
        self.name = name
        self.message_max_length = int(message_max_length)

    async def tcp_echo_client(self, message):
        """
        on client side send the message for echo
        """
        reader, writer = await asyncio.open_connection(self.ip, self.port)
        print(f'{self.name} send: {message!r}')
        writer.write(message.encode())

        data = await reader.read(self.message_max_length)
        print(f'{self.name} replied: {data.decode()}')

        print('close the socket')
        # The following lines closes the stream properly
        # If there is any warning, it's due to a bug o Python 3.8: https://bugs.python.org/issue38529
        # Please ignore it
        writer.close()

    def run_until_quit(self):
        # start the loop
        while True:
            # collect the message to send
            message = input("Please input the next message to send: ")
            if message in ['quit', 'exit', ':q', 'exit;', 'quit;', 'exit()', '(exit)']:
                break
            else:
                asyncio.run(self.tcp_echo_client(message))


if __name__ == '__main__':
    client = Client()  # using the default settings
    client.run_until_quit()
