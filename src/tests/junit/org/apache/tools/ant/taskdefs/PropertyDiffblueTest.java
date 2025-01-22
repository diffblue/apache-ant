package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.file.Paths;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Target;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Reference;
import org.junit.Test;

public class PropertyDiffblueTest {
  /**
   * Test {@link Property#Property()}.
   * <p>
   * Method under test: {@link Property#Property()}
   */
  @Test
  public void testNewProperty() {
    // Arrange and Act
    Property actualProperty = new Property();

    // Assert
    assertNull(actualProperty.getFile());
    assertNull(actualProperty.getDescription());
    assertNull(actualProperty.getTaskName());
    assertNull(actualProperty.getTaskType());
    assertNull(actualProperty.getEnvironment());
    assertNull(actualProperty.getName());
    assertNull(actualProperty.getPrefix());
    assertNull(actualProperty.getResource());
    assertNull(actualProperty.getRuntime());
    assertNull(actualProperty.getValue());
    assertNull(actualProperty.getUrl());
    assertNull(actualProperty.getProject());
    assertNull(actualProperty.getOwningTarget());
    assertNull(actualProperty.getClasspath());
    assertNull(actualProperty.getRefid());
    assertFalse(actualProperty.getPrefixValues());
    assertFalse(actualProperty.userProperty);
  }

  /**
   * Test {@link Property#Property(boolean)}.
   * <p>
   * Method under test: {@link Property#Property(boolean)}
   */
  @Test
  public void testNewProperty2() {
    // Arrange and Act
    Property actualProperty = new Property(true);

    // Assert
    assertNull(actualProperty.getFile());
    assertNull(actualProperty.getDescription());
    assertNull(actualProperty.getTaskName());
    assertNull(actualProperty.getTaskType());
    assertNull(actualProperty.getEnvironment());
    assertNull(actualProperty.getName());
    assertNull(actualProperty.getPrefix());
    assertNull(actualProperty.getResource());
    assertNull(actualProperty.getRuntime());
    assertNull(actualProperty.getValue());
    assertNull(actualProperty.getUrl());
    assertNull(actualProperty.getProject());
    assertNull(actualProperty.getOwningTarget());
    assertNull(actualProperty.getClasspath());
    assertNull(actualProperty.getRefid());
    assertFalse(actualProperty.getPrefixValues());
    assertTrue(actualProperty.userProperty);
  }

  /**
   * Test {@link Property#Property(boolean, Project)}.
   * <p>
   * Method under test: {@link Property#Property(boolean, Project)}
   */
  @Test
  public void testNewProperty3() {
    // Arrange and Act
    Property actualProperty = new Property(true, new Project());

    // Assert
    assertNull(actualProperty.getFile());
    assertNull(actualProperty.getDescription());
    assertNull(actualProperty.getTaskName());
    assertNull(actualProperty.getTaskType());
    assertNull(actualProperty.getEnvironment());
    assertNull(actualProperty.getName());
    assertNull(actualProperty.getPrefix());
    assertNull(actualProperty.getResource());
    assertNull(actualProperty.getRuntime());
    assertNull(actualProperty.getValue());
    assertNull(actualProperty.getUrl());
    assertNull(actualProperty.getProject());
    assertNull(actualProperty.getOwningTarget());
    assertNull(actualProperty.getClasspath());
    assertNull(actualProperty.getRefid());
    assertFalse(actualProperty.getPrefixValues());
    assertTrue(actualProperty.userProperty);
  }

  /**
   * Test {@link Property#setLocation(File)} with {@code File}.
   * <ul>
   *   <li>Given {@link Property#Property()} Relative is {@code true}.</li>
   *   <li>Then {@link Property#Property()} Value is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Property#setLocation(File)}
   */
  @Test
  public void testSetLocationWithFile_givenPropertyRelativeIsTrue_thenPropertyValueIsNull() {
    // Arrange
    Property property = new Property();
    property.setRelative(true);

    // Act
    property.setLocation((File) null);

    // Assert that nothing has changed
    assertNull(property.getValue());
  }

  /**
   * Test {@link Property#setLocation(File)} with {@code File}.
   * <ul>
   *   <li>Given {@link Property#Property()} Relative is {@code true}.</li>
   *   <li>Then {@link Property#Property()} Value is {@code /NULL_FILE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Property#setLocation(File)}
   */
  @Test
  public void testSetLocationWithFile_givenPropertyRelativeIsTrue_thenPropertyValueIsNullFile() {
    // Arrange
    Property property = new Property();
    property.setRelative(true);

    // Act
    property.setLocation(Copy.NULL_FILE_PLACEHOLDER);

    // Assert
    assertEquals("/NULL_FILE", property.getValue());
  }

  /**
   * Test {@link Property#setLocation(File)} with {@code File}.
   * <ul>
   *   <li>Given {@link Property#Property()}.</li>
   *   <li>Then {@link Property#Property()} Value is {@code /NULL_FILE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Property#setLocation(File)}
   */
  @Test
  public void testSetLocationWithFile_givenProperty_thenPropertyValueIsNullFile() {
    // Arrange
    Property property = new Property();

    // Act
    property.setLocation(Copy.NULL_FILE_PLACEHOLDER);

    // Assert
    assertEquals("/NULL_FILE", property.getValue());
  }

  /**
   * Test {@link Property#setValue(Object)} with {@code Object}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link Property#Property()} Value is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Property#setValue(Object)}
   */
  @Test
  public void testSetValueWithObject_whenNull_thenPropertyValueIsNull() {
    // Arrange
    Property property = new Property();

    // Act
    property.setValue((Object) null);

    // Assert that nothing has changed
    assertNull(property.getValue());
  }

  /**
   * Test {@link Property#setValue(Object)} with {@code Object}.
   * <ul>
   *   <li>When {@code Value}.</li>
   *   <li>Then {@link Property#Property()} Value is {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Property#setValue(Object)}
   */
  @Test
  public void testSetValueWithObject_whenValue_thenPropertyValueIsValue() {
    // Arrange
    Property property = new Property();

    // Act
    property.setValue((Object) "Value");

    // Assert
    assertEquals("Value", property.getValue());
  }

  /**
   * Test {@link Property#setValue(String)} with {@code String}.
   * <ul>
   *   <li>When {@code 42}.</li>
   *   <li>Then {@link Property#Property()} Value is {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Property#setValue(String)}
   */
  @Test
  public void testSetValueWithString_when42_thenPropertyValueIs42() {
    // Arrange
    Property property = new Property();

    // Act
    property.setValue("42");

    // Assert
    assertEquals("42", property.getValue());
  }

  /**
   * Test {@link Property#setValue(String)} with {@code String}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link Property#Property()} Value is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Property#setValue(String)}
   */
  @Test
  public void testSetValueWithString_whenNull_thenPropertyValueIsNull() {
    // Arrange
    Property property = new Property();

    // Act
    property.setValue((String) null);

    // Assert that nothing has changed
    assertNull(property.getValue());
  }

  /**
   * Test {@link Property#addText(String)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>When {@code Msg}.</li>
   *   <li>Then {@link Property#Property()} Value is {@code Msg}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Property#addText(String)}
   */
  @Test
  public void testAddText_givenJavaLangObject_whenMsg_thenPropertyValueIsMsg() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Adding reference: ", typeClass);
    project.addBuildListener(new AntClassLoader());

    Property property = new Property();
    property.setProject(project);

    // Act
    property.addText("Msg");

    // Assert
    assertEquals("Msg", property.getValue());
  }

  /**
   * Test {@link Property#addText(String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then {@link Property#Property()} Value is {@code Msg}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Property#addText(String)}
   */
  @Test
  public void testAddText_givenProjectAddBuildListenerAntClassLoader_thenPropertyValueIsMsg() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Property property = new Property();
    property.setProject(project);

    // Act
    property.addText("Msg");

    // Assert
    assertEquals("Msg", property.getValue());
  }

  /**
   * Test {@link Property#addText(String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code Adding reference:} and {@link Target#Target()}.</li>
   *   <li>Then {@link Property#Property()} Value is {@code Msg}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Property#addText(String)}
   */
  @Test
  public void testAddText_givenProjectAddTargetAddingReferenceAndTarget_thenPropertyValueIsMsg() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("Adding reference: ", new Target());
    project.addBuildListener(new AntClassLoader());

    Property property = new Property();
    property.setProject(project);

    // Act
    property.addText("Msg");

    // Assert
    assertEquals("Msg", property.getValue());
  }

  /**
   * Test {@link Property#addText(String)}.
   * <ul>
   *   <li>Given {@link Property#Property()} Project is {@link Project} (default constructor).</li>
   *   <li>When {@code Msg}.</li>
   *   <li>Then {@link Property#Property()} Value is {@code Msg}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Property#addText(String)}
   */
  @Test
  public void testAddText_givenPropertyProjectIsProject_whenMsg_thenPropertyValueIsMsg() {
    // Arrange
    Property property = new Property();
    property.setProject(new Project());

    // Act
    property.addText("Msg");

    // Assert
    assertEquals("Msg", property.getValue());
  }

  /**
   * Test {@link Property#addText(String)}.
   * <ul>
   *   <li>Given {@link Property#Property()} Value is {@code Value}.</li>
   *   <li>When empty string.</li>
   *   <li>Then {@link Property#Property()} Value is {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Property#addText(String)}
   */
  @Test
  public void testAddText_givenPropertyValueIsValue_whenEmptyString_thenPropertyValueIsValue() {
    // Arrange
    Property property = new Property();
    property.setValue((Object) "Value");

    // Act
    property.addText("");

    // Assert that nothing has changed
    assertEquals("Value", property.getValue());
  }

  /**
   * Test {@link Property#addText(String)}.
   * <ul>
   *   <li>Given {@link Property#Property()} Value is {@code Value}.</li>
   *   <li>When {@code Msg}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Property#addText(String)}
   */
  @Test
  public void testAddText_givenPropertyValueIsValue_whenMsg_thenThrowBuildException() {
    // Arrange
    Property property = new Property();
    property.setValue((Object) "Value");

    // Act and Assert
    assertThrows(BuildException.class, () -> property.addText("Msg"));
  }

  /**
   * Test {@link Property#setPrefix(String)}.
   * <ul>
   *   <li>When {@code .}.</li>
   *   <li>Then {@link Property#Property()} Prefix is {@code .}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Property#setPrefix(String)}
   */
  @Test
  public void testSetPrefix_whenDot_thenPropertyPrefixIsDot() {
    // Arrange
    Property property = new Property();

    // Act
    property.setPrefix(".");

    // Assert
    assertEquals(".", property.getPrefix());
  }

  /**
   * Test {@link Property#setPrefix(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link Property#Property()} Prefix is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Property#setPrefix(String)}
   */
  @Test
  public void testSetPrefix_whenNull_thenPropertyPrefixIsNull() {
    // Arrange
    Property property = new Property();

    // Act
    property.setPrefix(null);

    // Assert that nothing has changed
    assertNull(property.getPrefix());
  }

  /**
   * Test {@link Property#setPrefix(String)}.
   * <ul>
   *   <li>When {@code Prefix}.</li>
   *   <li>Then {@link Property#Property()} Prefix is {@code Prefix.}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Property#setPrefix(String)}
   */
  @Test
  public void testSetPrefix_whenPrefix_thenPropertyPrefixIsPrefix() {
    // Arrange
    Property property = new Property();

    // Act
    property.setPrefix("Prefix");

    // Assert
    assertEquals("Prefix.", property.getPrefix());
  }

  /**
   * Test {@link Property#setClasspath(Path)}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Property#setClasspath(Path)}
   */
  @Test
  public void testSetClasspath_whenNull() {
    // Arrange
    Property property = new Property();
    property.setClasspath(Path.systemBootClasspath);

    // Act
    property.setClasspath(null);

    // Assert that nothing has changed
    assertTrue(property.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
  }

  /**
   * Test {@link Property#createClasspath()}.
   * <ul>
   *   <li>Given {@link Property#Property()} Project is {@link Project} (default constructor).</li>
   *   <li>Then return Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Property#createClasspath()}
   */
  @Test
  public void testCreateClasspath_givenPropertyProjectIsProject_thenReturnProjectIsProject() {
    // Arrange
    Property property = new Property();
    Project project = new Project();
    property.setProject(project);

    // Act and Assert
    assertSame(project, property.createClasspath().getProject());
  }

  /**
   * Test {@link Property#createClasspath()}.
   * <ul>
   *   <li>Given {@link Property#Property()}.</li>
   *   <li>Then {@link Property#Property()} Classpath Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Property#createClasspath()}
   */
  @Test
  public void testCreateClasspath_givenProperty_thenPropertyClasspathDescriptionIsNull() {
    // Arrange
    Property property = new Property();

    // Act
    Path actualCreateClasspathResult = property.createClasspath();

    // Assert
    Path classpath = property.getClasspath();
    assertNull(classpath.getDescription());
    assertNull(actualCreateClasspathResult.getProject());
    assertNull(classpath.getProject());
    assertNull(classpath.getRefid());
    assertEquals(0, classpath.size());
    assertFalse(classpath.isReference());
    assertTrue(classpath.isEmpty());
  }

  /**
   * Test {@link Property#createClasspath()}.
   * <ul>
   *   <li>Then {@link Property#Property()} Classpath is {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Property#createClasspath()}
   */
  @Test
  public void testCreateClasspath_thenPropertyClasspathIsSystemBootClasspath() {
    // Arrange
    Property property = new Property();
    property.setClasspath(Path.systemBootClasspath);

    // Act and Assert
    Path expectedClasspath = property.createClasspath().systemBootClasspath;
    assertSame(expectedClasspath, property.getClasspath());
  }

  /**
   * Test {@link Property#setClasspathRef(Reference)}.
   * <ul>
   *   <li>Given {@link Property#Property()}.</li>
   *   <li>Then {@link Property#Property()} Classpath Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Property#setClasspathRef(Reference)}
   */
  @Test
  public void testSetClasspathRef_givenProperty_thenPropertyClasspathProjectIsNull() {
    // Arrange
    Property property = new Property();

    // Act
    property.setClasspathRef(new Reference("42"));

    // Assert
    Path classpath = property.getClasspath();
    assertNull(classpath.getDescription());
    assertNull(classpath.getProject());
    assertNull(classpath.getRefid());
    assertFalse(classpath.isReference());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Property#setBasedir(File)}
   *   <li>{@link Property#setEnvironment(String)}
   *   <li>{@link Property#setFile(File)}
   *   <li>{@link Property#setName(String)}
   *   <li>{@link Property#setPrefixValues(boolean)}
   *   <li>{@link Property#setRefid(Reference)}
   *   <li>{@link Property#setRelative(boolean)}
   *   <li>{@link Property#setResource(String)}
   *   <li>{@link Property#setRuntime(String)}
   *   <li>{@link Property#setUrl(URL)}
   *   <li>{@link Property#getClasspath()}
   *   <li>{@link Property#getEnvironment()}
   *   <li>{@link Property#getFile()}
   *   <li>{@link Property#getName()}
   *   <li>{@link Property#getPrefix()}
   *   <li>{@link Property#getPrefixValues()}
   *   <li>{@link Property#getRefid()}
   *   <li>{@link Property#getResource()}
   *   <li>{@link Property#getRuntime()}
   *   <li>{@link Property#getUrl()}
   *   <li>{@link Property#getValue()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() throws MalformedURLException {
    // Arrange
    Property property = new Property();

    // Act
    property.setBasedir(Copy.NULL_FILE_PLACEHOLDER);
    property.setEnvironment("Env");
    File file = Copy.NULL_FILE_PLACEHOLDER;
    property.setFile(file);
    property.setName(Manifest.ATTRIBUTE_NAME);
    property.setPrefixValues(true);
    Reference ref = new Reference("42");
    property.setRefid(ref);
    property.setRelative(true);
    property.setResource("Resource");
    property.setRuntime("Prefix");
    URL url = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL();
    property.setUrl(url);
    Path actualClasspath = property.getClasspath();
    String actualEnvironment = property.getEnvironment();
    File actualFile = property.getFile();
    String actualName = property.getName();
    String actualPrefix = property.getPrefix();
    boolean actualPrefixValues = property.getPrefixValues();
    Reference actualRefid = property.getRefid();
    String actualResource = property.getResource();
    String actualRuntime = property.getRuntime();
    URL actualUrl = property.getUrl();

    // Assert
    assertEquals("Env", actualEnvironment);
    assertEquals("Prefix", actualRuntime);
    assertEquals("Resource", actualResource);
    assertNull(actualPrefix);
    assertNull(property.getValue());
    assertNull(actualClasspath);
    assertTrue(actualPrefixValues);
    String expectedToStringResult = String.join("", "file:",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString().concat(File.separator));
    assertEquals(expectedToStringResult, actualUrl.toString());
    assertEquals(Manifest.ATTRIBUTE_NAME, actualName);
    assertSame(ref, actualRefid);
    assertSame(url, actualUrl);
    assertSame(file, actualFile);
  }

  /**
   * Test {@link Property#toString()}.
   * <ul>
   *   <li>Given {@link Property#Property()} Value is {@code Value}.</li>
   *   <li>Then return {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Property#toString()}
   */
  @Test
  public void testToString_givenPropertyValueIsValue_thenReturnValue() {
    // Arrange
    Property property = new Property();
    property.setValue((Object) "Value");

    // Act and Assert
    assertEquals("Value", property.toString());
  }

  /**
   * Test {@link Property#toString()}.
   * <ul>
   *   <li>Given {@link Property#Property()}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Property#toString()}
   */
  @Test
  public void testToString_givenProperty_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", (new Property()).toString());
  }

  /**
   * Test {@link Property#execute()}.
   * <p>
   * Method under test: {@link Property#execute()}
   */
  @Test
  public void testExecute() throws BuildException {
    // Arrange
    Property property = new Property();
    property.setName("You must specify url, file, resource, environment or runtime when not using the name attribute");
    property.setProject(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> property.execute());
  }

  /**
   * Test {@link Property#execute()}.
   * <p>
   * Method under test: {@link Property#execute()}
   */
  @Test
  public void testExecute2() throws BuildException {
    // Arrange
    Property property = new Property();
    property.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    property.setProject(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> property.execute());
  }

  /**
   * Test {@link Property#execute()}.
   * <ul>
   *   <li>Given {@link Property#Property()} Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Property#execute()}
   */
  @Test
  public void testExecute_givenPropertyProjectIsProject_thenThrowBuildException() throws BuildException {
    // Arrange
    Property property = new Property();
    property.setProject(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> property.execute());
  }

  /**
   * Test {@link Property#execute()}.
   * <ul>
   *   <li>Given {@link Property#Property()}.</li>
   *   <li>Then throw {@link IllegalStateException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Property#execute()}
   */
  @Test
  public void testExecute_givenProperty_thenThrowIllegalStateException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(IllegalStateException.class, () -> (new Property()).execute());
  }

  /**
   * Test {@link Property#loadUrl(URL)}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Property#loadUrl(URL)}
   */
  @Test
  public void testLoadUrl_thenThrowBuildException() throws MalformedURLException, BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class,
        () -> (new Property()).loadUrl(Paths.get(System.getProperty("java.io.tmpdir"), "Loading ").toUri().toURL()));
  }

  /**
   * Test {@link Property#loadFile(File)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Property#loadFile(File)}
   */
  @Test
  public void testLoadFile_whenPropertyIsJavaIoTmpdirIsTestTxtToFile_thenThrowBuildException() throws BuildException {
    // Arrange
    Property property = new Property();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> property.loadFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }
}
