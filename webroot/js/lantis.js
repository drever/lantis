(function (lantis, $, undefined) {
	lantis.createIssue = function (projectId){
	    $.ajax({
		url: "../createIssue/" + projectId,
		method: 'POST'
	    })
	     .done(function(data) {
                 $('#New').append(data);
	    });
	}

        lantis.deleteIssue = function (issueId){
            $.ajax({
                url: "../deleteIssue/" + issueId,
                method: 'POST'
            })
             .done(function(data) {
                 $('#issue' + data).remove();
            });
        }

	lantis.drag = function(event){
	    event.dataTransfer.setData("source-card", event.target.id);
	}

	lantis.allowDrag = function (event){
	    event.preventDefault();
	}

	lantis.drop = function (event){
	    var sourceCard = event.dataTransfer.getData("source-card");
	    var targetColumn = $(event.target).closest('.column');

	    $('#' + sourceCard).appendTo(targetColumn);
	}
}(window.lantis = window.lantis || {}, jQuery));
