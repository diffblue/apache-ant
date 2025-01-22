package org.apache.tools.ant.types;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.util.Iterator;
import java.util.Stack;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Target;
import org.apache.tools.ant.types.PropertySet.BuiltinPropertySetName;
import org.apache.tools.ant.types.PropertySet.PropertyRef;
import org.apache.tools.ant.types.mappers.CutDirsMapper;
import org.apache.tools.ant.types.resources.PropertyResource;
import org.apache.tools.ant.util.CompositeMapper;
import org.apache.tools.ant.util.FileNameMapper;
import org.apache.tools.ant.util.IdentityMapper;
import org.junit.Test;

public class PropertySetDiffblueTest {
  /**
   * Test {@link PropertySet#appendName(String)}.
   * <ul>
   *   <li>When {@code Name}.</li>
   *   <li>Then not {@link PropertySet} (default constructor) Checked.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#appendName(String)}
   */
  @Test
  public void testAppendName_whenName_thenNotPropertySetChecked() {
    // Arrange
    PropertySet propertySet = new PropertySet();

    // Act
    propertySet.appendName("Name");

    // Assert
    assertFalse(propertySet.isChecked());
  }

  /**
   * Test {@link PropertySet#appendPrefix(String)}.
   * <ul>
   *   <li>When {@code Prefix}.</li>
   *   <li>Then not {@link PropertySet} (default constructor) Checked.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#appendPrefix(String)}
   */
  @Test
  public void testAppendPrefix_whenPrefix_thenNotPropertySetChecked() {
    // Arrange
    PropertySet propertySet = new PropertySet();

    // Act
    propertySet.appendPrefix("Prefix");

    // Assert
    assertFalse(propertySet.isChecked());
  }

  /**
   * Test BuiltinPropertySetName {@link BuiltinPropertySetName#getValues()}.
   * <p>
   * Method under test: {@link BuiltinPropertySetName#getValues()}
   */
  @Test
  public void testBuiltinPropertySetNameGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"all", "system", "commandline"}, (new BuiltinPropertySetName()).getValues());
  }

  /**
   * Test BuiltinPropertySetName new {@link BuiltinPropertySetName} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link BuiltinPropertySetName}
   */
  @Test
  public void testBuiltinPropertySetNameNewBuiltinPropertySetName() {
    // Arrange and Act
    BuiltinPropertySetName actualBuiltinPropertySetName = new BuiltinPropertySetName();

    // Assert
    assertNull(actualBuiltinPropertySetName.getValue());
    assertEquals(-1, actualBuiltinPropertySetName.getIndex());
  }

  /**
   * Test PropertyRef getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link PropertyRef}
   *   <li>{@link PropertyRef#toString()}
   * </ul>
   */
  @Test
  public void testPropertyRefGettersAndSetters() {
    // Arrange, Act and Assert
    assertEquals("name=null, regex=null, prefix=null, builtin=null", (new PropertyRef()).toString());
  }

  /**
   * Test PropertyRef {@link PropertyRef#setBuiltin(BuiltinPropertySetName)}.
   * <ul>
   *   <li>When {@link BuiltinPropertySetName} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyRef#setBuiltin(BuiltinPropertySetName)}
   */
  @Test
  public void testPropertyRefSetBuiltin_whenBuiltinPropertySetName_thenThrowBuildException() {
    // Arrange
    PropertyRef propertyRef = new PropertyRef();

    // Act and Assert
    assertThrows(BuildException.class, () -> propertyRef.setBuiltin(new BuiltinPropertySetName()));
  }

  /**
   * Test PropertyRef {@link PropertyRef#setName(String)}.
   * <ul>
   *   <li>Given {@link PropertyRef} (default constructor) Regex is {@code .*}.</li>
   *   <li>When {@code Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyRef#setName(String)}
   */
  @Test
  public void testPropertyRefSetName_givenPropertyRefRegexIsDotAsterisk_whenName() {
    // Arrange
    PropertyRef propertyRef = new PropertyRef();
    propertyRef.setRegex(".*");

    // Act and Assert
    assertThrows(BuildException.class, () -> propertyRef.setName("Name"));
  }

  /**
   * Test PropertyRef {@link PropertyRef#setName(String)}.
   * <ul>
   *   <li>Given {@link PropertyRef} (default constructor).</li>
   *   <li>When empty string.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyRef#setName(String)}
   */
  @Test
  public void testPropertyRefSetName_givenPropertyRef_whenEmptyString_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new PropertyRef()).setName(""));
  }

  /**
   * Test PropertyRef {@link PropertyRef#setName(String)}.
   * <ul>
   *   <li>Given {@link PropertyRef} (default constructor).</li>
   *   <li>When {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyRef#setName(String)}
   */
  @Test
  public void testPropertyRefSetName_givenPropertyRef_whenNull_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new PropertyRef()).setName(null));
  }

  /**
   * Test PropertyRef {@link PropertyRef#setPrefix(String)}.
   * <ul>
   *   <li>Given {@link PropertyRef} (default constructor) Name is {@code prefix}.</li>
   *   <li>When {@code Prefix}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyRef#setPrefix(String)}
   */
  @Test
  public void testPropertyRefSetPrefix_givenPropertyRefNameIsPrefix_whenPrefix() {
    // Arrange
    PropertyRef propertyRef = new PropertyRef();
    propertyRef.setName("prefix");

    // Act and Assert
    assertThrows(BuildException.class, () -> propertyRef.setPrefix("Prefix"));
  }

  /**
   * Test PropertyRef {@link PropertyRef#setPrefix(String)}.
   * <ul>
   *   <li>Given {@link PropertyRef} (default constructor).</li>
   *   <li>When {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyRef#setPrefix(String)}
   */
  @Test
  public void testPropertyRefSetPrefix_givenPropertyRef_whenNull_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new PropertyRef()).setPrefix(null));
  }

  /**
   * Test PropertyRef {@link PropertyRef#setPrefix(String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyRef#setPrefix(String)}
   */
  @Test
  public void testPropertyRefSetPrefix_whenEmptyString_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new PropertyRef()).setPrefix(""));
  }

  /**
   * Test PropertyRef {@link PropertyRef#setRegex(String)}.
   * <ul>
   *   <li>Given {@link PropertyRef} (default constructor) Name is {@code regex}.</li>
   *   <li>When {@code .*}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyRef#setRegex(String)}
   */
  @Test
  public void testPropertyRefSetRegex_givenPropertyRefNameIsRegex_whenDotAsterisk() {
    // Arrange
    PropertyRef propertyRef = new PropertyRef();
    propertyRef.setName("regex");

    // Act and Assert
    assertThrows(BuildException.class, () -> propertyRef.setRegex(".*"));
  }

  /**
   * Test PropertyRef {@link PropertyRef#setRegex(String)}.
   * <ul>
   *   <li>Given {@link PropertyRef} (default constructor).</li>
   *   <li>When empty string.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyRef#setRegex(String)}
   */
  @Test
  public void testPropertyRefSetRegex_givenPropertyRef_whenEmptyString_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new PropertyRef()).setRegex(""));
  }

  /**
   * Test PropertyRef {@link PropertyRef#setRegex(String)}.
   * <ul>
   *   <li>Given {@link PropertyRef} (default constructor).</li>
   *   <li>When {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyRef#setRegex(String)}
   */
  @Test
  public void testPropertyRefSetRegex_givenPropertyRef_whenNull_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new PropertyRef()).setRegex(null));
  }

  /**
   * Test {@link PropertySet#setMapper(String, String, String)}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor) add {@link CutDirsMapper} (default constructor).</li>
   *   <li>When {@code Type}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#setMapper(String, String, String)}
   */
  @Test
  public void testSetMapper_givenPropertySetAddCutDirsMapper_whenType_thenThrowBuildException() {
    // Arrange
    PropertySet propertySet = new PropertySet();
    propertySet.add(new CutDirsMapper());

    // Act and Assert
    assertThrows(BuildException.class,
        () -> propertySet.setMapper("Type", "jane.doe@example.org", "alice.liddell@example.org"));
  }

  /**
   * Test {@link PropertySet#setMapper(String, String, String)}.
   * <ul>
   *   <li>When {@code identity}.</li>
   *   <li>Then {@link PropertySet} (default constructor) Mapper Implementation {@link IdentityMapper}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#setMapper(String, String, String)}
   */
  @Test
  public void testSetMapper_whenIdentity_thenPropertySetMapperImplementationIdentityMapper()
      throws ClassNotFoundException, BuildException {
    // Arrange
    PropertySet propertySet = new PropertySet();

    // Act
    propertySet.setMapper("identity", "jane.doe@example.org", "alice.liddell@example.org");

    // Assert
    Mapper mapper = propertySet.getMapper();
    assertTrue(mapper.getImplementation() instanceof IdentityMapper);
    assertEquals("Mapper", mapper.getDataTypeName());
    assertEquals("alice.liddell@example.org", mapper.to);
    assertEquals("jane.doe@example.org", mapper.from);
    assertNull(mapper.getDescription());
    assertNull(mapper.classname);
    assertNull(mapper.getProject());
    assertNull(mapper.classpath);
    assertNull(mapper.getRefid());
    assertFalse(propertySet.isChecked());
    assertFalse(mapper.isReference());
    assertTrue(mapper.isChecked());
    Class<IdentityMapper> expectedImplementationClass = IdentityMapper.class;
    assertEquals(expectedImplementationClass, mapper.getImplementationClass());
  }

  /**
   * Test {@link PropertySet#createMapper()}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor) add {@link CutDirsMapper} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#createMapper()}
   */
  @Test
  public void testCreateMapper_givenPropertySetAddCutDirsMapper_thenThrowBuildException() {
    // Arrange
    PropertySet propertySet = new PropertySet();
    propertySet.add(new CutDirsMapper());

    // Act and Assert
    assertThrows(BuildException.class, () -> propertySet.createMapper());
  }

  /**
   * Test {@link PropertySet#createMapper()}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor).</li>
   *   <li>Then return DataTypeName is {@code Mapper}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#createMapper()}
   */
  @Test
  public void testCreateMapper_givenPropertySet_thenReturnDataTypeNameIsMapper() {
    // Arrange
    PropertySet propertySet = new PropertySet();

    // Act
    Mapper actualCreateMapperResult = propertySet.createMapper();

    // Assert
    assertEquals("Mapper", actualCreateMapperResult.getDataTypeName());
    assertNull(actualCreateMapperResult.getDescription());
    assertNull(actualCreateMapperResult.classname);
    assertNull(actualCreateMapperResult.from);
    assertNull(actualCreateMapperResult.to);
    assertNull(actualCreateMapperResult.getProject());
    assertNull(actualCreateMapperResult.type);
    assertNull(actualCreateMapperResult.classpath);
    assertNull(actualCreateMapperResult.getRefid());
    assertFalse(propertySet.isChecked());
    assertFalse(actualCreateMapperResult.isReference());
    assertTrue(actualCreateMapperResult.isChecked());
  }

  /**
   * Test {@link PropertySet#add(FileNameMapper)}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor) add {@link CutDirsMapper} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#add(FileNameMapper)}
   */
  @Test
  public void testAdd_givenPropertySetAddCutDirsMapper_thenThrowBuildException() {
    // Arrange
    PropertySet propertySet = new PropertySet();
    propertySet.add(new CutDirsMapper());

    // Act and Assert
    assertThrows(BuildException.class, () -> propertySet.add(new CutDirsMapper()));
  }

  /**
   * Test {@link PropertySet#add(FileNameMapper)}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor).</li>
   *   <li>Then {@link PropertySet} (default constructor) Mapper Implementation {@link CompositeMapper}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#add(FileNameMapper)}
   */
  @Test
  public void testAdd_givenPropertySet_thenPropertySetMapperImplementationCompositeMapper() throws BuildException {
    // Arrange
    PropertySet propertySet = new PropertySet();

    // Act
    propertySet.add(new CutDirsMapper());

    // Assert
    Mapper mapper = propertySet.getMapper();
    assertTrue(mapper.getImplementation() instanceof CompositeMapper);
    assertEquals("Mapper", mapper.getDataTypeName());
    assertNull(mapper.getDescription());
    assertNull(mapper.classname);
    assertNull(mapper.from);
    assertNull(mapper.to);
    assertNull(mapper.getProject());
    assertNull(mapper.type);
    assertNull(mapper.classpath);
    assertNull(mapper.getRefid());
    assertFalse(propertySet.isChecked());
    assertFalse(mapper.isChecked());
    assertFalse(mapper.isReference());
  }

  /**
   * Test {@link PropertySet#getDynamic()}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#getDynamic()}
   */
  @Test
  public void testGetDynamic_givenPropertySet() {
    // Arrange, Act and Assert
    assertTrue((new PropertySet()).getDynamic());
  }

  /**
   * Test {@link PropertySet#getDynamic()}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor) appendName {@code Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#getDynamic()}
   */
  @Test
  public void testGetDynamic_givenPropertySetAppendNameName() {
    // Arrange
    PropertySet propertySet = new PropertySet();
    propertySet.appendName("Name");

    // Act and Assert
    assertTrue(propertySet.getDynamic());
  }

  /**
   * Test {@link PropertySet#getMapper()}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#getMapper()}
   */
  @Test
  public void testGetMapper_givenPropertySet() {
    // Arrange, Act and Assert
    assertNull((new PropertySet()).getMapper());
  }

  /**
   * Test {@link PropertySet#getMapper()}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor) appendName {@code Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#getMapper()}
   */
  @Test
  public void testGetMapper_givenPropertySetAppendNameName() {
    // Arrange
    PropertySet propertySet = new PropertySet();
    propertySet.appendName("Name");

    // Act and Assert
    assertNull(propertySet.getMapper());
  }

  /**
   * Test {@link PropertySet#getProperties()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#getProperties()}
   */
  @Test
  public void testGetProperties_givenProjectAddBuildListenerAntClassLoader_thenReturnEmpty() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    PropertySet propertySet = new PropertySet();
    propertySet.setProject(project);
    propertySet.appendName("Name");

    // Act and Assert
    assertTrue(propertySet.getProperties().isEmpty());
  }

  /**
   * Test {@link PropertySet#getProperties()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code Adding reference:} and {@link Target#Target()}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#getProperties()}
   */
  @Test
  public void testGetProperties_givenProjectAddTargetAddingReferenceAndTarget_thenReturnEmpty() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("Adding reference: ", new Target());
    project.addBuildListener(new AntClassLoader());

    PropertySet propertySet = new PropertySet();
    propertySet.setProject(project);
    propertySet.appendName("Name");

    // Act and Assert
    assertTrue(propertySet.getProperties().isEmpty());
  }

  /**
   * Test {@link PropertySet#getProperties()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) Default is {@code ant.PropertyHelper}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#getProperties()}
   */
  @Test
  public void testGetProperties_givenProjectDefaultIsAntPropertyHelper_thenReturnEmpty() {
    // Arrange
    Project project = new Project();
    project.setDefault("ant.PropertyHelper");
    project.addBuildListener(new AntClassLoader());

    PropertySet propertySet = new PropertySet();
    propertySet.setProject(project);
    propertySet.appendName("Name");

    // Act and Assert
    assertTrue(propertySet.getProperties().isEmpty());
  }

  /**
   * Test {@link PropertySet#getProperties()}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor) add {@link CutDirsMapper} (default constructor).</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#getProperties()}
   */
  @Test
  public void testGetProperties_givenPropertySetAddCutDirsMapper_thenReturnEmpty() {
    // Arrange
    PropertySet propertySet = new PropertySet();
    propertySet.add(new CutDirsMapper());
    propertySet.appendName("Name");

    // Act and Assert
    assertTrue(propertySet.getProperties().isEmpty());
  }

  /**
   * Test {@link PropertySet#getProperties()}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor) addPropertyset {@link PropertySet} (default constructor).</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#getProperties()}
   */
  @Test
  public void testGetProperties_givenPropertySetAddPropertysetPropertySet_thenReturnEmpty() {
    // Arrange
    PropertySet propertySet = new PropertySet();
    propertySet.addPropertyset(new PropertySet());
    propertySet.appendName("Name");

    // Act and Assert
    assertTrue(propertySet.getProperties().isEmpty());
  }

  /**
   * Test {@link PropertySet#getProperties()}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor) appendName {@code Name}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#getProperties()}
   */
  @Test
  public void testGetProperties_givenPropertySetAppendNameName_thenReturnEmpty() {
    // Arrange
    PropertySet propertySet = new PropertySet();
    propertySet.appendName("Name");

    // Act and Assert
    assertTrue(propertySet.getProperties().isEmpty());
  }

  /**
   * Test {@link PropertySet#getProperties()}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor) appendPrefix {@code all}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#getProperties()}
   */
  @Test
  public void testGetProperties_givenPropertySetAppendPrefixAll_thenReturnEmpty() {
    // Arrange
    PropertySet propertySet = new PropertySet();
    propertySet.appendPrefix("all");
    propertySet.appendName("Name");

    // Act and Assert
    assertTrue(propertySet.getProperties().isEmpty());
  }

  /**
   * Test {@link PropertySet#getProperties()}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor) appendRegex {@code Regex}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#getProperties()}
   */
  @Test
  public void testGetProperties_givenPropertySetAppendRegexRegex_thenReturnEmpty() {
    // Arrange
    PropertySet propertySet = new PropertySet();
    propertySet.appendRegex("Regex");
    propertySet.appendName("Name");

    // Act and Assert
    assertTrue(propertySet.getProperties().isEmpty());
  }

  /**
   * Test {@link PropertySet#getProperties()}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#getProperties()}
   */
  @Test
  public void testGetProperties_givenPropertySetProjectIsProject_thenReturnEmpty() {
    // Arrange
    PropertySet propertySet = new PropertySet();
    propertySet.setProject(new Project());
    propertySet.appendName("Name");

    // Act and Assert
    assertTrue(propertySet.getProperties().isEmpty());
  }

  /**
   * Test {@link PropertySet#getProperties()}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor).</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#getProperties()}
   */
  @Test
  public void testGetProperties_givenPropertySet_thenReturnEmpty() {
    // Arrange, Act and Assert
    assertTrue((new PropertySet()).getProperties().isEmpty());
  }

  /**
   * Test {@link PropertySet#setRefid(Reference)}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor).</li>
   *   <li>Then not {@link PropertySet} (default constructor) Checked.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#setRefid(Reference)}
   */
  @Test
  public void testSetRefid_givenPropertySet_thenNotPropertySetChecked() {
    // Arrange
    PropertySet propertySet = new PropertySet();
    Reference r = new Reference("42");

    // Act
    propertySet.setRefid(r);

    // Assert
    assertFalse(propertySet.isChecked());
    assertTrue(propertySet.isReference());
    assertSame(r, propertySet.getRefid());
  }

  /**
   * Test {@link PropertySet#toString()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#toString()}
   */
  @Test
  public void testToString_givenProjectAddBuildListenerAntClassLoader_thenReturnEmptyString() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    PropertySet propertySet = new PropertySet();
    propertySet.setProject(project);
    propertySet.appendName(", ");

    // Act and Assert
    assertEquals("", propertySet.toString());
  }

  /**
   * Test {@link PropertySet#toString()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code Adding reference:} and {@link Target#Target()}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#toString()}
   */
  @Test
  public void testToString_givenProjectAddTargetAddingReferenceAndTarget_thenReturnEmptyString() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("Adding reference: ", new Target());
    project.addBuildListener(new AntClassLoader());

    PropertySet propertySet = new PropertySet();
    propertySet.setProject(project);
    propertySet.appendName(", ");

    // Act and Assert
    assertEquals("", propertySet.toString());
  }

  /**
   * Test {@link PropertySet#toString()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) Default is {@code ant.PropertyHelper}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#toString()}
   */
  @Test
  public void testToString_givenProjectDefaultIsAntPropertyHelper_thenReturnEmptyString() {
    // Arrange
    Project project = new Project();
    project.setDefault("ant.PropertyHelper");
    project.addBuildListener(new AntClassLoader());

    PropertySet propertySet = new PropertySet();
    propertySet.setProject(project);
    propertySet.appendName(", ");

    // Act and Assert
    assertEquals("", propertySet.toString());
  }

  /**
   * Test {@link PropertySet#toString()}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor) add {@link CutDirsMapper} (default constructor).</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#toString()}
   */
  @Test
  public void testToString_givenPropertySetAddCutDirsMapper_thenReturnEmptyString() {
    // Arrange
    PropertySet propertySet = new PropertySet();
    propertySet.add(new CutDirsMapper());
    propertySet.appendName(", ");

    // Act and Assert
    assertEquals("", propertySet.toString());
  }

  /**
   * Test {@link PropertySet#toString()}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor) addPropertyset {@link PropertySet} (default constructor).</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#toString()}
   */
  @Test
  public void testToString_givenPropertySetAddPropertysetPropertySet_thenReturnEmptyString() {
    // Arrange
    PropertySet propertySet = new PropertySet();
    propertySet.addPropertyset(new PropertySet());
    propertySet.appendName(", ");

    // Act and Assert
    assertEquals("", propertySet.toString());
  }

  /**
   * Test {@link PropertySet#toString()}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor) appendName {@code ,}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#toString()}
   */
  @Test
  public void testToString_givenPropertySetAppendNameComma_thenReturnEmptyString() {
    // Arrange
    PropertySet propertySet = new PropertySet();
    propertySet.appendName(", ");

    // Act and Assert
    assertEquals("", propertySet.toString());
  }

  /**
   * Test {@link PropertySet#toString()}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor) appendPrefix {@code Prefix}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#toString()}
   */
  @Test
  public void testToString_givenPropertySetAppendPrefixPrefix_thenReturnEmptyString() {
    // Arrange
    PropertySet propertySet = new PropertySet();
    propertySet.appendPrefix("Prefix");
    propertySet.appendName(", ");

    // Act and Assert
    assertEquals("", propertySet.toString());
  }

  /**
   * Test {@link PropertySet#toString()}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor) appendRegex {@code ,}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#toString()}
   */
  @Test
  public void testToString_givenPropertySetAppendRegexComma_thenReturnEmptyString() {
    // Arrange
    PropertySet propertySet = new PropertySet();
    propertySet.appendRegex(", ");
    propertySet.appendName(", ");

    // Act and Assert
    assertEquals("", propertySet.toString());
  }

  /**
   * Test {@link PropertySet#toString()}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#toString()}
   */
  @Test
  public void testToString_givenPropertySetProjectIsProject_thenReturnEmptyString() {
    // Arrange
    PropertySet propertySet = new PropertySet();
    propertySet.setProject(new Project());
    propertySet.appendName(", ");

    // Act and Assert
    assertEquals("", propertySet.toString());
  }

  /**
   * Test {@link PropertySet#toString()}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor).</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#toString()}
   */
  @Test
  public void testToString_givenPropertySet_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", (new PropertySet()).toString());
  }

  /**
   * Test {@link PropertySet#iterator()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then return not hasNext.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#iterator()}
   */
  @Test
  public void testIterator_givenProjectAddBuildListenerAntClassLoader_thenReturnNotHasNext() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    PropertySet propertySet = new PropertySet();
    propertySet.setProject(project);
    propertySet.appendName("Name");

    // Act and Assert
    assertFalse(propertySet.iterator().hasNext());
  }

  /**
   * Test {@link PropertySet#iterator()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code Adding reference:} and {@link Target#Target()}.</li>
   *   <li>Then return not hasNext.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#iterator()}
   */
  @Test
  public void testIterator_givenProjectAddTargetAddingReferenceAndTarget_thenReturnNotHasNext() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("Adding reference: ", new Target());
    project.addBuildListener(new AntClassLoader());

    PropertySet propertySet = new PropertySet();
    propertySet.setProject(project);
    propertySet.appendName("Name");

    // Act and Assert
    assertFalse(propertySet.iterator().hasNext());
  }

  /**
   * Test {@link PropertySet#iterator()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) Default is {@code ant.PropertyHelper}.</li>
   *   <li>Then return not hasNext.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#iterator()}
   */
  @Test
  public void testIterator_givenProjectDefaultIsAntPropertyHelper_thenReturnNotHasNext() {
    // Arrange
    Project project = new Project();
    project.setDefault("ant.PropertyHelper");
    project.addBuildListener(new AntClassLoader());

    PropertySet propertySet = new PropertySet();
    propertySet.setProject(project);
    propertySet.appendName("Name");

    // Act and Assert
    assertFalse(propertySet.iterator().hasNext());
  }

  /**
   * Test {@link PropertySet#iterator()}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor) add {@link CutDirsMapper} (default constructor).</li>
   *   <li>Then return not hasNext.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#iterator()}
   */
  @Test
  public void testIterator_givenPropertySetAddCutDirsMapper_thenReturnNotHasNext() {
    // Arrange
    PropertySet propertySet = new PropertySet();
    propertySet.add(new CutDirsMapper());
    propertySet.appendName("Name");

    // Act and Assert
    assertFalse(propertySet.iterator().hasNext());
  }

  /**
   * Test {@link PropertySet#iterator()}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor) add {@link CutDirsMapper} (default constructor).</li>
   *   <li>Then return not hasNext.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#iterator()}
   */
  @Test
  public void testIterator_givenPropertySetAddCutDirsMapper_thenReturnNotHasNext2() {
    // Arrange
    PropertySet ref = new PropertySet();
    ref.add(new CutDirsMapper());

    PropertySet propertySet = new PropertySet();
    propertySet.addPropertyset(ref);
    propertySet.appendName("Name");

    // Act and Assert
    assertFalse(propertySet.iterator().hasNext());
  }

  /**
   * Test {@link PropertySet#iterator()}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor) addPropertyset {@link PropertySet} (default constructor).</li>
   *   <li>Then return not hasNext.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#iterator()}
   */
  @Test
  public void testIterator_givenPropertySetAddPropertysetPropertySet_thenReturnNotHasNext() {
    // Arrange
    PropertySet propertySet = new PropertySet();
    propertySet.addPropertyset(new PropertySet());
    propertySet.appendName("Name");

    // Act and Assert
    assertFalse(propertySet.iterator().hasNext());
  }

  /**
   * Test {@link PropertySet#iterator()}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor) appendName {@code Name}.</li>
   *   <li>Then return not hasNext.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#iterator()}
   */
  @Test
  public void testIterator_givenPropertySetAppendNameName_thenReturnNotHasNext() {
    // Arrange
    PropertySet propertySet = new PropertySet();
    propertySet.appendName("Name");

    // Act and Assert
    assertFalse(propertySet.iterator().hasNext());
  }

  /**
   * Test {@link PropertySet#iterator()}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor) appendPrefix {@code all}.</li>
   *   <li>Then return not hasNext.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#iterator()}
   */
  @Test
  public void testIterator_givenPropertySetAppendPrefixAll_thenReturnNotHasNext() {
    // Arrange
    PropertySet propertySet = new PropertySet();
    propertySet.appendPrefix("all");
    propertySet.appendName("Name");

    // Act and Assert
    assertFalse(propertySet.iterator().hasNext());
  }

  /**
   * Test {@link PropertySet#iterator()}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor) appendRegex {@code Regex}.</li>
   *   <li>Then return not hasNext.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#iterator()}
   */
  @Test
  public void testIterator_givenPropertySetAppendRegexRegex_thenReturnNotHasNext() {
    // Arrange
    PropertySet propertySet = new PropertySet();
    propertySet.appendRegex("Regex");
    propertySet.appendName("Name");

    // Act and Assert
    assertFalse(propertySet.iterator().hasNext());
  }

  /**
   * Test {@link PropertySet#iterator()}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor) Negate is {@code true}.</li>
   *   <li>Then next return {@link PropertyResource}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#iterator()}
   */
  @Test
  public void testIterator_givenPropertySetNegateIsTrue_thenNextReturnPropertyResource() {
    // Arrange
    PropertySet propertySet = new PropertySet();
    propertySet.setNegate(true);
    propertySet.appendName("Name");

    // Act
    Iterator<Resource> actualIteratorResult = propertySet.iterator();

    // Assert
    assertTrue(actualIteratorResult.next() instanceof PropertyResource);
    assertTrue(actualIteratorResult.next() instanceof PropertyResource);
    assertTrue(actualIteratorResult.next() instanceof PropertyResource);
    assertTrue(actualIteratorResult.next() instanceof PropertyResource);
    assertTrue(actualIteratorResult.next() instanceof PropertyResource);
    assertTrue(actualIteratorResult.next() instanceof PropertyResource);
    assertTrue(actualIteratorResult.next() instanceof PropertyResource);
    assertTrue(actualIteratorResult.next() instanceof PropertyResource);
    assertTrue(actualIteratorResult.next() instanceof PropertyResource);
    assertTrue(actualIteratorResult.next() instanceof PropertyResource);
    assertTrue(actualIteratorResult.hasNext());
  }

  /**
   * Test {@link PropertySet#iterator()}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return not hasNext.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#iterator()}
   */
  @Test
  public void testIterator_givenPropertySetProjectIsProject_thenReturnNotHasNext() {
    // Arrange
    PropertySet propertySet = new PropertySet();
    propertySet.setProject(new Project());
    propertySet.appendName("Name");

    // Act and Assert
    assertFalse(propertySet.iterator().hasNext());
  }

  /**
   * Test {@link PropertySet#iterator()}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor).</li>
   *   <li>Then return not hasNext.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#iterator()}
   */
  @Test
  public void testIterator_givenPropertySet_thenReturnNotHasNext() {
    // Arrange, Act and Assert
    assertFalse((new PropertySet()).iterator().hasNext());
  }

  /**
   * Test {@link PropertySet#iterator()}.
   * <ul>
   *   <li>Then next return {@link PropertyResource}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#iterator()}
   */
  @Test
  public void testIterator_thenNextReturnPropertyResource() {
    // Arrange
    PropertySet propertySet = new PropertySet();
    propertySet.appendRegex(".*");

    // Act
    Iterator<Resource> actualIteratorResult = propertySet.iterator();

    // Assert
    assertTrue(actualIteratorResult.next() instanceof PropertyResource);
    assertTrue(actualIteratorResult.next() instanceof PropertyResource);
    assertTrue(actualIteratorResult.next() instanceof PropertyResource);
    assertTrue(actualIteratorResult.next() instanceof PropertyResource);
    assertTrue(actualIteratorResult.next() instanceof PropertyResource);
    assertTrue(actualIteratorResult.next() instanceof PropertyResource);
    assertTrue(actualIteratorResult.next() instanceof PropertyResource);
    assertTrue(actualIteratorResult.next() instanceof PropertyResource);
    assertTrue(actualIteratorResult.next() instanceof PropertyResource);
    assertTrue(actualIteratorResult.next() instanceof PropertyResource);
    assertTrue(actualIteratorResult.hasNext());
  }

  /**
   * Test {@link PropertySet#iterator()}.
   * <ul>
   *   <li>Then next return {@link PropertyResource}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#iterator()}
   */
  @Test
  public void testIterator_thenNextReturnPropertyResource2() {
    // Arrange
    PropertySet ref = new PropertySet();
    ref.appendRegex(".*");

    PropertySet propertySet = new PropertySet();
    propertySet.addPropertyset(ref);
    propertySet.appendName("Name");

    // Act
    Iterator<Resource> actualIteratorResult = propertySet.iterator();

    // Assert
    assertTrue(actualIteratorResult.next() instanceof PropertyResource);
    assertTrue(actualIteratorResult.next() instanceof PropertyResource);
    assertTrue(actualIteratorResult.next() instanceof PropertyResource);
    assertTrue(actualIteratorResult.next() instanceof PropertyResource);
    assertTrue(actualIteratorResult.next() instanceof PropertyResource);
    assertTrue(actualIteratorResult.next() instanceof PropertyResource);
    assertTrue(actualIteratorResult.next() instanceof PropertyResource);
    assertTrue(actualIteratorResult.next() instanceof PropertyResource);
    assertTrue(actualIteratorResult.next() instanceof PropertyResource);
    assertTrue(actualIteratorResult.next() instanceof PropertyResource);
    assertTrue(actualIteratorResult.hasNext());
  }

  /**
   * Test {@link PropertySet#size()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#size()}
   */
  @Test
  public void testSize_givenProjectAddBuildListenerAntClassLoader_thenReturnZero() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    PropertySet propertySet = new PropertySet();
    propertySet.setProject(project);
    propertySet.appendName("Name");

    // Act and Assert
    assertEquals(0, propertySet.size());
  }

  /**
   * Test {@link PropertySet#size()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code Adding reference:} and {@link Target#Target()}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#size()}
   */
  @Test
  public void testSize_givenProjectAddTargetAddingReferenceAndTarget_thenReturnZero() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("Adding reference: ", new Target());
    project.addBuildListener(new AntClassLoader());

    PropertySet propertySet = new PropertySet();
    propertySet.setProject(project);
    propertySet.appendName("Name");

    // Act and Assert
    assertEquals(0, propertySet.size());
  }

  /**
   * Test {@link PropertySet#size()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) Default is {@code ant.PropertyHelper}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#size()}
   */
  @Test
  public void testSize_givenProjectDefaultIsAntPropertyHelper_thenReturnZero() {
    // Arrange
    Project project = new Project();
    project.setDefault("ant.PropertyHelper");
    project.addBuildListener(new AntClassLoader());

    PropertySet propertySet = new PropertySet();
    propertySet.setProject(project);
    propertySet.appendName("Name");

    // Act and Assert
    assertEquals(0, propertySet.size());
  }

  /**
   * Test {@link PropertySet#size()}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor) add {@link CutDirsMapper} (default constructor).</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#size()}
   */
  @Test
  public void testSize_givenPropertySetAddCutDirsMapper_thenReturnZero() {
    // Arrange
    PropertySet propertySet = new PropertySet();
    propertySet.add(new CutDirsMapper());
    propertySet.appendName("Name");

    // Act and Assert
    assertEquals(0, propertySet.size());
  }

  /**
   * Test {@link PropertySet#size()}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor) addPropertyset {@link PropertySet} (default constructor).</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#size()}
   */
  @Test
  public void testSize_givenPropertySetAddPropertysetPropertySet_thenReturnZero() {
    // Arrange
    PropertySet propertySet = new PropertySet();
    propertySet.addPropertyset(new PropertySet());
    propertySet.appendName("Name");

    // Act and Assert
    assertEquals(0, propertySet.size());
  }

  /**
   * Test {@link PropertySet#size()}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor) appendName {@code Name}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#size()}
   */
  @Test
  public void testSize_givenPropertySetAppendNameName_thenReturnZero() {
    // Arrange
    PropertySet propertySet = new PropertySet();
    propertySet.appendName("Name");

    // Act and Assert
    assertEquals(0, propertySet.size());
  }

  /**
   * Test {@link PropertySet#size()}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor) appendPrefix {@code all}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#size()}
   */
  @Test
  public void testSize_givenPropertySetAppendPrefixAll_thenReturnZero() {
    // Arrange
    PropertySet propertySet = new PropertySet();
    propertySet.appendPrefix("all");
    propertySet.appendName("Name");

    // Act and Assert
    assertEquals(0, propertySet.size());
  }

  /**
   * Test {@link PropertySet#size()}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor) appendRegex {@code Regex}.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#size()}
   */
  @Test
  public void testSize_givenPropertySetAppendRegexRegex_thenReturnZero() {
    // Arrange
    PropertySet propertySet = new PropertySet();
    propertySet.appendRegex("Regex");
    propertySet.appendName("Name");

    // Act and Assert
    assertEquals(0, propertySet.size());
  }

  /**
   * Test {@link PropertySet#size()}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#size()}
   */
  @Test
  public void testSize_givenPropertySetProjectIsProject_thenReturnZero() {
    // Arrange
    PropertySet propertySet = new PropertySet();
    propertySet.setProject(new Project());
    propertySet.appendName("Name");

    // Act and Assert
    assertEquals(0, propertySet.size());
  }

  /**
   * Test {@link PropertySet#size()}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor).</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#size()}
   */
  @Test
  public void testSize_givenPropertySet_thenReturnZero() {
    // Arrange, Act and Assert
    assertEquals(0, (new PropertySet()).size());
  }

  /**
   * Test {@link PropertySet#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenPropertySet() {
    // Arrange, Act and Assert
    assertFalse((new PropertySet()).isFilesystemOnly());
  }

  /**
   * Test {@link PropertySet#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor) appendName {@code Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenPropertySetAppendNameName() {
    // Arrange
    PropertySet propertySet = new PropertySet();
    propertySet.appendName("Name");

    // Act and Assert
    assertFalse(propertySet.isFilesystemOnly());
  }

  /**
   * Test {@link PropertySet#dieOnCircularReference(Stack, Project)} with {@code stk}, {@code p}.
   * <ul>
   *   <li>Given {@code 42}.</li>
   *   <li>When {@link Stack} (default constructor) add {@code 42}.</li>
   *   <li>Then {@link PropertySet} (default constructor) Checked.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#dieOnCircularReference(Stack, Project)}
   */
  @Test
  public void testDieOnCircularReferenceWithStkP_given42_whenStackAdd42_thenPropertySetChecked() throws BuildException {
    // Arrange
    PropertySet propertySet = new PropertySet();

    Stack<Object> stk = new Stack<>();
    stk.add("42");

    // Act
    propertySet.dieOnCircularReference(stk, new Project());

    // Assert that nothing has changed
    assertTrue(propertySet.isChecked());
  }

  /**
   * Test {@link PropertySet#dieOnCircularReference(Stack, Project)} with {@code stk}, {@code p}.
   * <ul>
   *   <li>Given {@code 42}.</li>
   *   <li>When {@link Stack} (default constructor) add {@code 42}.</li>
   *   <li>Then {@link PropertySet} (default constructor) Checked.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#dieOnCircularReference(Stack, Project)}
   */
  @Test
  public void testDieOnCircularReferenceWithStkP_given42_whenStackAdd42_thenPropertySetChecked2()
      throws BuildException {
    // Arrange
    PropertySet propertySet = new PropertySet();

    Stack<Object> stk = new Stack<>();
    stk.add("42");
    stk.add("42");

    // Act
    propertySet.dieOnCircularReference(stk, new Project());

    // Assert that nothing has changed
    assertTrue(propertySet.isChecked());
  }

  /**
   * Test {@link PropertySet#dieOnCircularReference(Stack, Project)} with {@code stk}, {@code p}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor) add {@link CutDirsMapper} (default constructor).</li>
   *   <li>When {@link Stack} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#dieOnCircularReference(Stack, Project)}
   */
  @Test
  public void testDieOnCircularReferenceWithStkP_givenPropertySetAddCutDirsMapper_whenStack() throws BuildException {
    // Arrange
    PropertySet propertySet = new PropertySet();
    propertySet.add(new CutDirsMapper());
    propertySet.appendName("Name");
    Stack<Object> stk = new Stack<>();

    // Act
    propertySet.dieOnCircularReference(stk, new Project());

    // Assert
    assertTrue(propertySet.isChecked());
  }

  /**
   * Test {@link PropertySet#dieOnCircularReference(Stack, Project)} with {@code stk}, {@code p}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor) addPropertyset {@link PropertySet} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#dieOnCircularReference(Stack, Project)}
   */
  @Test
  public void testDieOnCircularReferenceWithStkP_givenPropertySetAddPropertysetPropertySet() throws BuildException {
    // Arrange
    PropertySet propertySet = new PropertySet();
    propertySet.addPropertyset(new PropertySet());
    propertySet.appendName("Name");
    Stack<Object> stk = new Stack<>();

    // Act
    propertySet.dieOnCircularReference(stk, new Project());

    // Assert
    assertTrue(propertySet.isChecked());
  }

  /**
   * Test {@link PropertySet#dieOnCircularReference(Stack, Project)} with {@code stk}, {@code p}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor) appendName {@code Name}.</li>
   *   <li>When {@link Stack} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#dieOnCircularReference(Stack, Project)}
   */
  @Test
  public void testDieOnCircularReferenceWithStkP_givenPropertySetAppendNameName_whenStack() throws BuildException {
    // Arrange
    PropertySet propertySet = new PropertySet();
    propertySet.appendName("Name");
    Stack<Object> stk = new Stack<>();

    // Act
    propertySet.dieOnCircularReference(stk, new Project());

    // Assert
    assertTrue(propertySet.isChecked());
  }

  /**
   * Test {@link PropertySet#dieOnCircularReference(Stack, Project)} with {@code stk}, {@code p}.
   * <ul>
   *   <li>Given {@link PropertySet} (default constructor).</li>
   *   <li>When {@link Stack} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertySet#dieOnCircularReference(Stack, Project)}
   */
  @Test
  public void testDieOnCircularReferenceWithStkP_givenPropertySet_whenStack() throws BuildException {
    // Arrange
    PropertySet propertySet = new PropertySet();
    Stack<Object> stk = new Stack<>();

    // Act
    propertySet.dieOnCircularReference(stk, new Project());

    // Assert that nothing has changed
    assertTrue(propertySet.isChecked());
  }

  /**
   * Test new {@link PropertySet} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link PropertySet}
   */
  @Test
  public void testNewPropertySet() {
    // Arrange and Act
    PropertySet actualPropertySet = new PropertySet();

    // Assert
    assertEquals("PropertySet", actualPropertySet.getDataTypeName());
    assertNull(actualPropertySet.getDescription());
    assertNull(actualPropertySet.getProject());
    assertNull(actualPropertySet.getMapper());
    assertNull(actualPropertySet.getRefid());
    assertEquals(0, actualPropertySet.size());
    assertFalse(actualPropertySet.isReference());
    assertTrue(actualPropertySet.getProperties().isEmpty());
    assertTrue(actualPropertySet.isChecked());
    assertTrue(actualPropertySet.getDynamic());
    assertTrue(actualPropertySet.isEmpty());
  }
}
