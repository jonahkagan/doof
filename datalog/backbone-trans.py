from pyDatalog import pyDatalog

##################################################
# Facts

pyDatalog.create_atoms('has_field', 'model')

+ has_field('spec1', 'a', 't')
+ has_field('spec1', 'b', 's')
+ has_field('spec1', 'id', 'string')
+ model('spec1', 'model1')

+ has_field('spec2', 'x', 'bool')
+ model('spec2', 'model2')

+ model('spec3', 'model3')

##################################################
# Rules

pyDatalog.create_atoms('Spec', 'Model', 'Attrs', 'A', 'B', 'T', 'S')

pyDatalog.create_atoms('attrs')
+ attrs('model1', 'attrs1')
+ attrs('model2', 'attrs2')
+ attrs('model3', 'attrs3')

# Model should have a 'cid' field
has_field(Model, 'cid', 'string') <= \
    model(Spec, Model)

# Model should have 'attributes' field that has all the fields of spec
has_field(Model, 'attributes', Attrs) <= \
    attrs(Model, Attrs)

has_field(Attrs, A, T) <= \
    model(Spec, Model) & \
    has_field(Model, 'attributes', Attrs) & \
    has_field(Spec, A, T)

# Model should have 'id' field if spec does
has_field(Model, 'id', T) <= \
    model(Spec, Model) & \
    has_field(Spec, 'id', T)

##################################################
# Queries

print 'has_field(model1, A, T)'
print(has_field('model1', A, T))
print
print 'has_field(attrs1, B, S)'
print(has_field('attrs1', B, S))
print
print 'has_field(model2, A, T)'
print(has_field('model2', A, T))
print
print 'has_field(attrs2, B, S)'
print(has_field('attrs2', B, S))
print
print 'has_field(model3, A, T)'
print(has_field('model3', A, T))
print
print 'has_field(attrs3, B, S)'
print(has_field('attrs3', B, S))
print
