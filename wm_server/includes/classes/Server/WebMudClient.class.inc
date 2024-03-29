<?php
	/*******************************************************************************************************************
	 * 
	 * This work is licensed under the Creative Commons Attribution 3.0 United States License. 
	 * To view a copy of this license, visit http://creativecommons.org/licenses/by/3.0/us/ or 
	 * send a letter to Creative Commons, 171 Second Street, Suite 300, San Francisco, California, 94105, USA.
	 * 
	 *******************************************************************************************************************/

class WebMudClient extends SocketClient
{
	private $mud_conn;
	private $connected_mud;	
	private $buffer;
	private $read_count;
	
	private $lastInput;	
	private $clientType = GC_WEBSOCKET;
	
	private $logging = false;
	private $log;	
	
	private $wm_no_negotiate;
	
	private $last_map;

	function getLastMap() {return $this->last_map_url;}
	function setLastMap($url) {$this->last_map_url = $url;}
		
	function startLogging()
	{
		$this->logging = true;		
	}
	
	function stopLogging()
	{
		$this->logging = false;
		$this->log = "";
	} 
	
	function isLogging()
	{
		return $this->logging;
	}
	
	function getLog()
	{
		return $this->log;
	}
	
	function addLog($data)
	{
		if ($this->isLogging())
			$this->log .= $data;
	}
	
	function setMudConnection($c)
	{
		$this->mud_conn = $c;
		$this->connected_mud = true;
	}
	
	function getMudConnection()
	{
		return $this->mud_conn;
	}
	
	function connectedMUD()
	{
		return $this->connected_mud;
	}
	
	function setDisconnectedMUD()
	{
		$this->connected_mud = false;
		unset($this->mud_conn);
	}
	
	function setNoNegotiate()
	{
		$this->wm_no_negotiate = true;
	}
	
	function isNoNegotiate()
	{
		return $this->wm_no_negotiate;
	}
	
	function addToBuffer($string)
	{
		$this->buffer .= $string;
		
		return $this->buffer;
	}
	
	function getBuffer()
	{
		return $this->buffer;
	}
	
	function clearBuffer()
	{
		$this->buffer = "";
		$this->read_count = 0;
	}
	
	function getReadCount()
	{
		return $this->read_count;
	}
	
	function bumpReadCount()
	{
		$this->read_count++;
	}
	
	function setClientType($type)
	{
		$this->clientType = $type;
	}
	
	function getClientType()
	{
		return $this->clientType;
	}
	
	function getLastTime()
	{
		return $this->lastInput;
	}
	
	function setLastTime()
	{
		$this->lastInput = microtime(true);
	}
	
	function send($buf)
	{		
		if ($buf != "") // New messages to send //
	    {		    			
			if (is_resource($this->socket)) 
			{
//				print "Sending:\n$buf\n";
//  print "ClientType: $this->clientType\n";
				
				if ($this->clientType == GC_FLASH)
				{					
		        	socket_write($this->socket, $buf . chr(0), strlen($buf . chr(0)));		        	
				} elseif ($this->clientType == GC_WEBSOCKET)					
				{								
					//$text = base64_encode($buf);
					$text = $buf;
    					// 0x1 text frame (FIN + opcode)
    					$b1 = 0x80 | (0x1 & 0x0f);
    					$length = strlen($text);

    					if ($length <= 125)
        					$header = pack('CC', $b1, $length);
    					elseif ($length > 125 && $length < 65536)
       						$header = pack('CCn', $b1, 126, $length);
    					else 
        					$header = pack('CCN', $b1, 127, $length);
					if ($header != "~")
					{
						$send=$header.$text;
//    print "Header:\n$header\n";
//    print "Sending:\n$send\n";
    						socket_write($this->socket, $send, strlen($send));
					}
				} elseif ($this->clientType ==GC_WEBSOCKETOLD)
				{
//   print "Sending Old:\n$buf\n";
  					socket_write($this->socket, chr(0) . $buf . chr(255), strlen(chr(0) . $buf . chr(255)));
				}				
			}			
	    	}
	}
}
?>
