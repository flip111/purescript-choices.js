"use strict";

require('choices.js/public/assets/scripts/choices.min.js');

exports.new = element => {
  return options => {
    // let opts = require('../../output/Chosen.Chosen').optionsToObject(options);
    let opts = {}

    return () => {
      console.log(Choices);
      let choices = new Choices(element, {addItems: true});
      return choices;
    }
  }
};

exports.destroy = element => { return () => { element.destroy(); }};
exports.init = element => { return () => { element.init(); }};
exports.highlightAll = element => { return () => { element.highlightAll(); }};
exports.unhighlightAll = element => { return () => { element.unhighlightAll(); }};
exports.removeActiveItemsByValue = element => { return value => { return () => { element.removeActiveItemsByValue(value); }}};
exports.removeActiveItems = element => { return excludedId => { return () => { element.removeActiveItems(excludedId); }}};
exports.removeHighlightedItems = element => { return () => { element.removeHighlightedItems(); }};
exports.showDropdown = element => { return () => { element.showDropdown(); }};
exports.hideDropdown = element => { return () => { element.hideDropdown(); }};

exports.clearChoices = element => { return () => { element.clearChoices(); }};
exports.getValueValue = element => { return () => { return element.getValue(true); }};
exports.getValueObject = element => { return () => { return element.getValue(); }};
exports.setValue = element => { return items => { return () => { element.setValue(items); }}};
exports.setChoiceByValue = element => { return value => { return () => { element.setChoiceByValue(value); }}};
exports.clearStore = element => { return () => { element.clearStore(); }};
exports.clearInput = element => { return () => { element.clearInput(); }};
exports.disable = element => { return () => { element.disable(); }};
exports.enable = element => { return () => { element.enable(); }};

exports.setChoices = element => {
  return choices => {
    return value => {
      return label => {
        return replaceChoices => {
          return () => { element.setChoices(choices, value, label, replaceChoices);
        }
      }
    }
  }
}};
