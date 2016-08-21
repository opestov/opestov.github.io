(ns o8v.game)

(defonce style
  (let [h (.item (js/document.getElementsByTagName "head") 0)
        s (js/document.createElement "style")]
    (.appendChild h s)
    s))

(set! (.-innerHTML style)
"
html {font-size: 62.5%;}
body {
  margin: 0;
  padding: 0;
  font: normal 2rem / 1.5 Helvetica, Arial, sans-serif;
  text-rendering: optimizeLegibility;
	background: white;
  color: #222;
}

h1 {
  margin: 0 0 2rem 0;
}
p {
  margin: 1rem 0 0 0;
}

.hatapp {
  width: 100%;
  max-width: 400px;
  margin: 0 auto;
  padding: 1rem 2rem;
  box-sizing: border-box;
  background-color: #f5f5f5;
}

button {
  display: block;
  padding: 0.5rem 1rem;
  margin: 1rem 0;
  width: 100%;
  background-color: #dadada;
  color: inherit;
	font-size: inherit;
	border: 3px dotted white;
	box-sizing: border-box;
  font-family: inherit;
  font-weight: inherit;
  text-align: left;
}

button:hover  {
  color: black;
	border: 3px solid white;
}

.go {
  font-weight: 600;
  font-size: 6rem;
}

.back {
  color: crimson;
  margin: 0 1rem 0 0;
  text-decoration: none;

}

.textbox {
	margin: 0.5rem 0 0 0;
  width: 100%;
  color: inherit;
	font-size: inherit;
	font-family: inherit;
	font-weight: inherit;
	padding: 0.5rem;
	border: 1px solid #999;
	box-sizing: border-box;
}

select {
  margin: 0;
  padding: 0.5rem 0.25rem;
  display: block;
  color: inherit;
	font-size: inherit;
	font-family: inherit;
	font-weight: inherit;
  width: 100%; 
	box-sizing: border-box;
}

.players { list-style: none; margin: 0; padding: 0; }
.players li { position: relative; padding: 0; border-top: 1px solid #ddd; }
.players li:last-child { border-bottom: 1px solid #ddd; }
.players li label{ display:block;  margin: 0 7rem 0 0; line-height: 3.5rem; }
.up-btn, .del-btn {
  position: absolute;
  margin: auto 0;
  font-size: 1.4rem;
  width: 3rem;
  height: 3rem;
  top: 0;
  bottom: 0;
  border: none;
}
.up-btn:hover, .del-btn:hover { border: none; color: crimson; }
.up-btn {  right: 3.5rem; }
.del-btn { right: 0rem; }

.word {
  background-color: crimson;
  color: white;
  text-align: center;
  padding: 0;
  border: none;
}
.word:hover  {
  border: none;
  background-color: crimson;
  color: white;
}

.progress {
  background-color: black;
  height: 1rem;
}

")
