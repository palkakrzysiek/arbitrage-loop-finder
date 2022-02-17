#!/usr/bin/env bash

#cp src/main/scala/ArbitrageOpportunitiesFinder.scala ArbitrageOpportunitiesFinder.sc

sed '/^package.*/d' src/main/scala/ArbitrageOpportunitiesFinder.scala | sed '/./,$!d' > ArbitrageOpportunitiesFinder.sc

echo -e "\nArbitrageOpportunitiesFinder.main(Array())" >> ArbitrageOpportunitiesFinder.sc

