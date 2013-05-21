shadowsocks-erlang
===========
Totally asynchronous implementation of Shadowsocks in Erlang.

Original python version: https://github.com/clowwindy/shadowsocks

Usage
-----------
* Compile

    `./rebar clean & ./rebar compile`

  Of course, Elang OTP is needed, I am using R15B01, other versions should works fine(maybe should 
  use your own rebar)
* Client side

    `erl -config ./local.config -pa ./ebin/ -boot start_sasl -s shadowsocks_app start -detached -noinput -noshell`
* Server side
   
   `erl -config ./remote.config -pa ./ebin/ -boot start_sasl -s shadowsocks_app start -detached -noinput -noshell`

Configuration
-----------
* Config files

    `local.config, remote.config`
* Descriptions

```erlang
     {shadowsocks,
     [
       {type, local},
       {local_port,1080},
       {server,"localhost"},
       {server_port,8388},
       {password,"barfoo!"},
       {method,default}
     ]}
```

`{type, local}` is for client, `{type, remote}` is for server side.

`{error_logger, {file, "log/shadowsocks_client.log"}}`, logs will be written 
in `log/shadowsocks_client.log`. If you don't want it, use `{error_logger, silent}` instead

TODOs
-----------
* Check IPv6
* Compatible with RC4 method

License
-----------
You are free to use, reuse and abuse all the code in this project, with following conditions are met:.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
