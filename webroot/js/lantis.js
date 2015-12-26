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

        lantis.editIssue = function (issueId) {
            $.ajax({
                url: "../issueEdit/" + issueId,
                method: 'GET'
            })
             .done(function(data) {
                  lantis.showIssue(data);
            });
        }

        lantis.showIssue = function (issue) {
             $('#content').append(issue);
        }
  
        lantis.hideIssue = function () {
             $('.edit').remove();
        }

        lantis.issueIdFromCard = function (issue) {
             return $(issue).attr('id').split('issue')[1];
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

            var issueId = sourceCard.split("issue")[1];
            var newStatus = $(targetColumn).prop('id');
            $.ajax({
                 url: "../setIssueStatus/" + issueId + "?status=" + newStatus,
                 method: 'POST'
            })
             .done(function (data) {
                 $('#' + sourceCard).remove();
                 $(data).appendTo(targetColumn);
            }); 
	}

    lantis.setIssueCategory = function (event){
        var newCategory = $("option:selected").text();
        var issueId = $(event).closest(".edit").attr("id");
        $.ajax({
            url: "../setIssueCategory/" + issueId + "?category=" + newCategory,
            method: 'POST'
        })
         .done(function (data) {
             //console.log(data);
         });
    }

}(window.lantis = window.lantis || {}, jQuery));
