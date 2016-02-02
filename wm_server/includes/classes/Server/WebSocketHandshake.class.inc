<?php

class WebSocketHandshake {

    /*! Easy way to handshake a WebSocket via draft-ietf-hybi-thewebsocketprotocol-00
     * @link    http://www.ietf.org/id/draft-ietf-hybi-thewebsocketprotocol-00.txt
     * @author  Andrea Giammarchi
     * @blog    webreflection.blogspot.com
     * @date    4th June 2010
     * @example
     *          // via function call ...
     *          $handshake = WebSocketHandshake($buffer);
     *          // ... or via class
     *          $handshake = (string)new WebSocketHandshake($buffer);
     *
     *          socket_write($socket, $handshake, strlen($handshake));
     */

    private $__value__;

    public function __construct($buffer) {
        $resource = $host = $upgrade = $connection = $origin = $key = $protocol = $version = $code = $handshake = $key1 = $key2 = null;
        preg_match('#GET (.*?) HTTP#', $buffer, $match) && $resource = $match[1];
        preg_match("#Host: (.*?)\r\n#", $buffer, $match) && $host = $match[1];
        preg_match("#Upgrade: (.*?)\r\n#", $buffer, $match) && $upgrade = $match[1];
        preg_match("#Connection: (.*?)\r\n#", $buffer, $match) && $connection = $match[1];
        preg_match("#Sec-WebSocket-Key: (.*?)\r\n#", $buffer, $match) && $key = $match[1];

        preg_match("#Sec-Websocket-Origin: (.*?)\r\n#", $buffer, $match) && $origin = $match[1];
        preg_match("#Sec-WebSocket-Protocol: (.*?)\r\n#", $buffer, $match) && $protocol = $match[1];
        preg_match("#Sec-WebSocket-Version: (.*?)\r\n#", $buffer, $match) && $version = $match[1];        

//        print "Handshake\n$buffer\n";
	if ($version == "13")
	{
	        $GUID			= "258EAFA5-E914-47DA-95CA-C5AB0DC85B11";
	        $ConcatKey		= $key.$GUID;
	        $SHA1			= sha1($ConcatKey,TRUE);
	        $accept	                = base64_encode($SHA1);

	        $this->__value__ =
        	       	"HTTP/1.1 101 Switching Protocols\r\n"				.
		       	"Upgrade: WebSocket\r\n"						.
		       	"Connection: Upgrade\r\n"					.
	     		"Sec-WebSocket-Accept: {$accept}\r\n\r\n"		;
	}
	else if ($version == "")
	{
        	preg_match("#Sec-WebSocket-Key1: (.*?)\r\n#", $buffer, $match) && $key1 = $match[1];
	        preg_match("#Sec-WebSocket-Key2: (.*?)\r\n#", $buffer, $match) && $key2 = $match[1];
        	preg_match("#Origin: (.*?)\r\n#", $buffer, $match) && $origin = $match[1];
	        preg_match("#\r\n(.*?)\$#", $buffer, $match) && $code = $match[1];

//		print "KEY1: $key1\n";
//		print "KEY2: $key2\n";
//		print "Code: $code\n";
  	      	$this->__value__ =
            		"HTTP/1.1 101 WebSocket Protocol Handshake\r\n".
            		"Upgrade: WebSocket\r\n".
            		"Connection: Upgrade\r\n".
            		"Sec-WebSocket-Origin: {$origin}\r\n".
            		"Sec-WebSocket-Location: ws://{$host}{$resource}\r\n".
            		($protocol ? "Sec-WebSocket-Protocol: {$protocol}\r\n" : "").
            		"\r\n".
            		$this->_createHandshakeThingy($key1, $key2, $code);
	} else
	{ 
		print "PROTOCOL ERROR: ".$buffer;
		$this->__value__ = "ERROR with protocol".$buffer;
	}
    }

    public function __toString() {
        return $this->__value__;
    }
    
    private function _doStuffToObtainAnInt32($key) {
        return preg_match_all('#[0-9]#', $key, $number) && preg_match_all('# #', $key, $space) ?
            implode('', $number[0]) / count($space[0]) :
            ''
        ;
    }

    private function _createHandshakeThingy($key1, $key2, $code) {
        return md5(
            pack('N', $this->_doStuffToObtainAnInt32($key1)).
            pack('N', $this->_doStuffToObtainAnInt32($key2)).
            $code,
            true
        );
    }
}

// handshake headers strings factory
function WebSocketHandshake($buffer) {
    return (string)new WebSocketHandshake($buffer);
}

?>
