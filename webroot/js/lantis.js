
function drag(event){
    event.dataTransfer.setData("source-card", event.target.id);
}

function allowDrag(event){
    event.preventDefault();
}

function drop(event){
    var sourceCard = event.dataTransfer.getData("source-card");
    var targetColumn = $(event.target).closest('.column');

    $('#' + sourceCard).appendTo(targetColumn);
}
