/*******************************************************************************************************************
 *
 * This work is licensed under the Creative Commons Attribution 3.0 United States License.
 * To view a copy of this license, visit http://creativecommons.org/licenses/by/3.0/us/ or
 * send a letter to Creative Commons, 171 Second Street, Suite 300, San Francisco, California, 94105, USA.
 *
 *******************************************************************************************************************/

var socket;

var num_msgs;
var next_del;

// Limit for messages displayed in the output window (will trim from the beginning past limit) //
//  - Should be kept reasonably small (< 100-ish) to prevent
//    the browser from clogging on all of the data
var msg_limit = 50;

var stopped;
var logging = false;
var output_mouse_over = false;
var smooth_scroll = false;

// Change this as necessary //
var wshost = "ws://hayseed.net:12346/phudbase-wm/wm_server/server.php";

$(document).ready(function(){
    num_msgs = 0;
    next_del = 0;

    socket = new WebSocket(wshost);

    socket.onmessage = function(evt) {
        handle_read(evt.data);
    };
    socket.onerror = function() {
        ow_Write("<p>WebSocket error:</p>");
    };
    socket.onopen = function() {
        if (sendDirect('PHUD:CONNECT ' + $('#mhost').val() + " " + $('#mport').val())) {
            postLogin();
        }
    };
    $('#data_form').submit( function() {
        send();
        return false;
    });
    $(window).resize(function() {
        $("#interface").height($(window).height() - $('#interface').offset().top);
        $("#output, #scroller, #right").height($("#interface").height() - 60);
    }).resize();
});

function sendDirect(data)
{
    if (data != "")
    {
        socket.send(data);
        return true;
    } else {
        return false;
    }
}

function send()
{
    s = document.getElementById("user_input").value;

    // print(s, "#999");

    socket.send(s);

    document.getElementById("user_input").value = "";
    return true;
}

function postLogin()
{
    $("#user_input").val('');
    $("#data_form").fadeIn(500, function() {
        $('#user_input').focus()
    });
    $("#login_form").remove();
}

function print(s) {

    var color = "#ccc";
    if (typeof(arguments[1]) != undefined)
        color = arguments[1];

    ow_Write("<br><span style='color:"+color+ "'>&rarr; " + s + "</span><br>");
}

function handle_read(s)
{
    data = eval("(" + s + ")");

    // Output a standard message //
    if (data.message) ow_Write(data.message);

    // Write a WebMud server status message //
    if (data.server_status) ss_Write(data.server_status);
}

function ow_Write(text)
{
    var objDiv = window.top.document.getElementById("output");

    objDiv.innerHTML += text;

    trim_ow();

    num_msgs++;

    objDiv.scrollTop = objDiv.scrollHeight;
}

function trim_ow()
{
    var elem;

    if (num_msgs >= msg_limit)
    {
        elem = "#msg" + next_del;

        $(elem).remove();

        next_del++;
    }
}
