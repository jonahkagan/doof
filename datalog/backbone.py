from pyDatalog import pyDatalog

##################################################
# Facts

pyDatalog.create_atoms('has_field', 'spec', 'model')

# well formed
+ spec('spec1')
+ has_field('spec1', 'a', 't')
+ has_field('spec1', 'b', 's')
+ has_field('spec1', 'id', 'string')

# ill formed - id has wrong type
+ spec('spec2')
+ has_field('spec2', 'a', 't')
+ has_field('spec2', 'b', 's')
+ has_field('spec2', 'id', 't')

# well formed
+ model('model1-1')
+ has_field('model1-1', 'attributes', 'attrs1-1')
+ has_field('model1-1', 'cid', 'string')
+ has_field('model1-1', 'id', 'string')

+ has_field('attrs1-1', 'a', 't')
+ has_field('attrs1-1', 'b', 's')
+ has_field('attrs1-1', 'id', 'string')

# ill formed - missing fields from attrs
+ model('model1-2')
+ has_field('model1-2', 'attributes', 'attrs1-2')

+ has_field('attrs1-2', 'a', 't')

# ill formed - missing id field
+ model('model1-3')
+ has_field('model1-3', 'attributes', 'attrs1-1')
+ has_field('model1-3', 'cid', 'string')

# well formed (but has no id field)
+ spec('spec3')

# well formed
+ model('model3-1')
+ has_field('model3-1', 'attributes', 'attrs3-1')
+ has_field('model3-1', 'cid', 'string')

# ill formed - has id field even though spec doesn't
+ model('model3-2')
+ has_field('model3-2', 'attributes', 'attrs3-1')
+ has_field('model3-2', 'cid', 'string')
+ has_field('model3-2', 'id', 'string')


##################################################
# Rules

pyDatalog.create_atoms('ill_formed_spec', 'ill_formed_model',
'ill_formed_pair', 'well_formed_pair', \
    'SPEC', 'MODEL', 'ATTRS', 'A', 'T', 'S')

# If spec has an 'id' field, it should have type string
ill_formed_spec(SPEC) <= \
    spec(SPEC) & \
    has_field(SPEC, 'id', T) & \
    (T != 'string')

# Model should have a 'cid' field
ill_formed_model(MODEL) <= \
    model(MODEL) & \
    ~ has_field(MODEL, 'cid', 'string')

# Model should have 'attributes' field that has all the fields of spec
ill_formed_pair(SPEC, MODEL) <= \
    spec(SPEC) & model(MODEL) & \
    has_field(SPEC, A, T) & \
    has_field(MODEL, 'attributes', ATTRS) & \
    ~ has_field(ATTRS, A, T)

# Model should have 'attributes' field that has only the fields of the spec
ill_formed_pair(SPEC, MODEL) <= \
    spec(SPEC) & model(MODEL) & \
    has_field(MODEL, 'attributes', ATTRS) & \
    has_field(ATTRS, A, T) & \
    ~ has_field(SPEC, A, T)

# Model should have 'id' field if spec does
ill_formed_pair(SPEC, MODEL) <= \
    spec(SPEC) & model(MODEL) & \
    has_field(SPEC, 'id', T) & \
    ~ has_field(MODEL, 'id', T)

# Model shouldn't have 'id' field if spec doesn't
ill_formed_pair(SPEC, MODEL) <= \
    spec(SPEC) & model(MODEL) & \
    ~ has_field(SPEC, 'id', T) & \
    has_field(MODEL, 'id', S)

well_formed_pair(SPEC, MODEL) <= \
    spec(SPEC) & model(MODEL) & \
    ~ ill_formed_spec(SPEC) & \
    ~ ill_formed_model(MODEL) & \
    ~ ill_formed_pair(SPEC, MODEL)

##################################################
# Queries

print 'ill_formed_spec'
print(ill_formed_spec(SPEC))
print
print 'ill_formed_model'
print(ill_formed_model(MODEL))
print
print 'ill_formed_pair'
print(ill_formed_pair(SPEC, MODEL))
print
print 'well_formed_pair'
print(well_formed_pair(SPEC, MODEL))
print
