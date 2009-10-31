<!DOCTYPE HTML>
<html>
<head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
    <title>{% block title %}Playdar{% endblock %}</title>
    <link rel="stylesheet" href="/static/playdar.css" type="text/css">
</head>
<body id="{% block bodyid %}playdar{% endblock %}">
    <a href="/" title="Playdar - Music Content Resolver" id="head">
        <img alt="Playdar - the Music Content Resolver" src="/static/playdar_auth_logo.gif" width="233" height="68"/>
    </a>
    
    <div id="content">
        {% block nav %}
        <p id="nav">
            <a href="/">Home</a> |
            <a href="/queries">Queries</a> |
            <a href="/authcodes">Authcodes</a> 
            &nbsp; || &nbsp;
            <a href="http://www.playdar.org/" target="x">www.playdar.org</a>
        </p>
        {% endblock %}
        
{% block content %}{% endblock %}
        
    </div>
</body>
</html>