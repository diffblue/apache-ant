package org.apache.tools.ant.types.resources;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.IOException;
import java.util.Hashtable;
import java.util.Map;
import java.util.Vector;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.BuildListener;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Target;
import org.apache.tools.ant.types.Reference;
import org.apache.tools.ant.types.Resource;
import org.junit.Test;

public class StringResourceDiffblueTest {
  /**
   * Test {@link StringResource#StringResource(Project, String)}.
   * <ul>
   *   <li>Given {@link Target#Target()}.</li>
   *   <li>Then return Project Targets size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringResource#StringResource(Project, String)}
   */
  @Test
  public void testNewStringResource_givenTarget_thenReturnProjectTargetsSizeIsOne() throws BuildException {
    // Arrange
    Project project = new Project();
    Target target = new Target();
    project.addTarget("ant.PropertyHelper", target);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    Project project2 = (new StringResource(project, "42")).getProject();
    Hashtable<String, Target> targets = project2.getTargets();
    assertEquals(1, targets.size());
    Map<String, Target> copyOfTargets = project2.getCopyOfTargets();
    assertEquals(1, copyOfTargets.size());
    assertEquals(1, project2.getBuildListeners().size());
    assertSame(target, targets.get("ant.PropertyHelper"));
    assertSame(target, copyOfTargets.get("ant.PropertyHelper"));
  }

  /**
   * Test {@link StringResource#StringResource(Project, String)}.
   * <ul>
   *   <li>Then return Project BuildListeners first is {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringResource#StringResource(Project, String)}
   */
  @Test
  public void testNewStringResource_thenReturnProjectBuildListenersFirstIsAntClassLoader() {
    // Arrange
    Project project = new Project();
    AntClassLoader listener = new AntClassLoader();
    project.addBuildListener(listener);

    // Act and Assert
    Project project2 = (new StringResource(project, "42")).getProject();
    Vector<BuildListener> buildListeners = project2.getBuildListeners();
    assertEquals(1, buildListeners.size());
    assertTrue(project2.getDataTypeDefinitions().isEmpty());
    assertTrue(project2.getTargets().isEmpty());
    Map<String, Class<?>> copyOfDataTypeDefinitions = project2.getCopyOfDataTypeDefinitions();
    assertTrue(copyOfDataTypeDefinitions.isEmpty());
    assertTrue(project2.getCopyOfTargets().isEmpty());
    assertEquals(copyOfDataTypeDefinitions, project2.getFilters());
    assertEquals(copyOfDataTypeDefinitions, project2.getInheritedProperties());
    assertEquals(copyOfDataTypeDefinitions, project2.getTaskDefinitions());
    assertEquals(copyOfDataTypeDefinitions, project2.getUserProperties());
    assertSame(listener, buildListeners.get(0));
  }

  /**
   * Test {@link StringResource#StringResource(Project, String)}.
   * <ul>
   *   <li>Then return Project DataTypeDefinitions size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringResource#StringResource(Project, String)}
   */
  @Test
  public void testNewStringResource_thenReturnProjectDataTypeDefinitionsSizeIsOne() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant.PropertyHelper", typeClass);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    Project project2 = (new StringResource(project, "42")).getProject();
    Hashtable<String, Class<?>> dataTypeDefinitions = project2.getDataTypeDefinitions();
    assertEquals(1, dataTypeDefinitions.size());
    Map<String, Class<?>> copyOfDataTypeDefinitions = project2.getCopyOfDataTypeDefinitions();
    assertEquals(1, copyOfDataTypeDefinitions.size());
    assertEquals(1, project2.getBuildListeners().size());
    Class<Object> expectedGetResult = Object.class;
    assertEquals(expectedGetResult, copyOfDataTypeDefinitions.get("ant.PropertyHelper"));
    assertSame(typeClass, dataTypeDefinitions.get("ant.PropertyHelper"));
  }

  /**
   * Test {@link StringResource#StringResource(Project, String)}.
   * <ul>
   *   <li>When {@link Project} (default constructor).</li>
   *   <li>Then return Project BuildListeners Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringResource#StringResource(Project, String)}
   */
  @Test
  public void testNewStringResource_whenProject_thenReturnProjectBuildListenersEmpty() {
    // Arrange, Act and Assert
    Project project = (new StringResource(new Project(), "42")).getProject();
    assertTrue(project.getDataTypeDefinitions().isEmpty());
    assertTrue(project.getTargets().isEmpty());
    Map<String, Class<?>> copyOfDataTypeDefinitions = project.getCopyOfDataTypeDefinitions();
    assertTrue(copyOfDataTypeDefinitions.isEmpty());
    assertTrue(project.getCopyOfTargets().isEmpty());
    assertTrue(project.getBuildListeners().isEmpty());
    assertEquals(copyOfDataTypeDefinitions, project.getFilters());
    assertEquals(copyOfDataTypeDefinitions, project.getInheritedProperties());
    assertEquals(copyOfDataTypeDefinitions, project.getTaskDefinitions());
    assertEquals(copyOfDataTypeDefinitions, project.getUserProperties());
  }

  /**
   * Test {@link StringResource#setName(String)}.
   * <ul>
   *   <li>Given {@link StringResource#StringResource(String)} with value is {@code 42}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringResource#setName(String)}
   */
  @Test
  public void testSetName_givenStringResourceWithValueIs42_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new StringResource("42")).setName("foo"));
  }

  /**
   * Test {@link StringResource#setName(String)}.
   * <ul>
   *   <li>Then {@link StringResource#StringResource()} toLongString is {@code StringResource "foo"}.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringResource#setName(String)}
   */
  @Test
  public void testSetName_thenStringResourceToLongStringIsStringResourceFoo() throws IOException {
    // Arrange
    StringResource stringResource = new StringResource();

    // Act
    stringResource.setName("foo");

    // Assert
    assertEquals("StringResource \"foo\"", stringResource.toLongString());
    assertEquals("foo", stringResource.getContent());
    assertEquals("foo", stringResource.getName());
    assertEquals("foo", stringResource.getValue());
    byte[] byteArray = new byte[3];
    assertEquals(3, stringResource.getInputStream().read(byteArray));
    assertEquals(3L, stringResource.getSize());
    assertTrue(stringResource.isExists());
    assertArrayEquals("foo".getBytes("UTF-8"), byteArray);
  }

  /**
   * Test {@link StringResource#setValue(String)}.
   * <ul>
   *   <li>Given {@link StringResource#StringResource()} Name is {@code foo}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringResource#setValue(String)}
   */
  @Test
  public void testSetValue_givenStringResourceNameIsFoo_thenThrowBuildException() {
    // Arrange
    StringResource stringResource = new StringResource();
    stringResource.setName("foo");

    // Act and Assert
    assertThrows(BuildException.class, () -> stringResource.setValue("foo"));
  }

  /**
   * Test {@link StringResource#setValue(String)}.
   * <ul>
   *   <li>Then {@link StringResource#StringResource()} toLongString is {@code StringResource "foo"}.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringResource#setValue(String)}
   */
  @Test
  public void testSetValue_thenStringResourceToLongStringIsStringResourceFoo() throws IOException {
    // Arrange
    StringResource stringResource = new StringResource();

    // Act
    stringResource.setValue("foo");

    // Assert
    assertEquals("StringResource \"foo\"", stringResource.toLongString());
    assertEquals("foo", stringResource.getContent());
    assertEquals("foo", stringResource.getName());
    assertEquals("foo", stringResource.getValue());
    byte[] byteArray = new byte[3];
    assertEquals(3, stringResource.getInputStream().read(byteArray));
    assertEquals(3L, stringResource.getSize());
    assertTrue(stringResource.isExists());
    assertArrayEquals("foo".getBytes("UTF-8"), byteArray);
  }

  /**
   * Test {@link StringResource#getName()}.
   * <p>
   * Method under test: {@link StringResource#getName()}
   */
  @Test
  public void testGetName() {
    // Arrange, Act and Assert
    assertNull((new StringResource()).getName());
  }

  /**
   * Test {@link StringResource#getValue()}.
   * <p>
   * Method under test: {@link StringResource#getValue()}
   */
  @Test
  public void testGetValue() {
    // Arrange, Act and Assert
    assertNull((new StringResource()).getValue());
  }

  /**
   * Test {@link StringResource#isExists()}.
   * <ul>
   *   <li>Given {@link StringResource#StringResource()} Name is {@code foo}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringResource#isExists()}
   */
  @Test
  public void testIsExists_givenStringResourceNameIsFoo_thenReturnTrue() {
    // Arrange
    StringResource stringResource = new StringResource();
    stringResource.setName("foo");

    // Act and Assert
    assertTrue(stringResource.isExists());
  }

  /**
   * Test {@link StringResource#isExists()}.
   * <ul>
   *   <li>Given {@link StringResource#StringResource()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringResource#isExists()}
   */
  @Test
  public void testIsExists_givenStringResource_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new StringResource()).isExists());
  }

  /**
   * Test {@link StringResource#addText(String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringResource#addText(String)}
   */
  @Test
  public void testAddText_givenProjectAddBuildListenerAntClassLoader() throws IOException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    StringResource stringResource = new StringResource();
    stringResource.setProject(project);

    // Act
    stringResource.addText("Text");

    // Assert
    assertEquals("StringResource \"Text\"", stringResource.toLongString());
    assertEquals("Text", stringResource.getContent());
    assertEquals("Text", stringResource.getName());
    assertEquals("Text", stringResource.getValue());
    byte[] byteArray = new byte[4];
    assertEquals(4, stringResource.getInputStream().read(byteArray));
    assertEquals(4L, stringResource.getSize());
    assertTrue(stringResource.isExists());
    assertArrayEquals("Text".getBytes("UTF-8"), byteArray);
  }

  /**
   * Test {@link StringResource#addText(String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code Adding reference:} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringResource#addText(String)}
   */
  @Test
  public void testAddText_givenProjectAddTargetAddingReferenceAndTarget() throws IOException, BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("Adding reference: ", new Target());
    project.addBuildListener(new AntClassLoader());

    StringResource stringResource = new StringResource();
    stringResource.setProject(project);

    // Act
    stringResource.addText("Text");

    // Assert
    assertEquals("StringResource \"Text\"", stringResource.toLongString());
    assertEquals("Text", stringResource.getContent());
    assertEquals("Text", stringResource.getName());
    assertEquals("Text", stringResource.getValue());
    byte[] byteArray = new byte[4];
    assertEquals(4, stringResource.getInputStream().read(byteArray));
    assertEquals(4L, stringResource.getSize());
    assertTrue(stringResource.isExists());
    assertArrayEquals("Text".getBytes("UTF-8"), byteArray);
  }

  /**
   * Test {@link StringResource#addText(String)}.
   * <ul>
   *   <li>Then {@link StringResource#StringResource()} toLongString is {@code StringResource "Text"}.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringResource#addText(String)}
   */
  @Test
  public void testAddText_thenStringResourceToLongStringIsStringResourceText() throws IOException {
    // Arrange
    StringResource stringResource = new StringResource();
    stringResource.setProject(new Project());

    // Act
    stringResource.addText("Text");

    // Assert
    assertEquals("StringResource \"Text\"", stringResource.toLongString());
    assertEquals("Text", stringResource.getContent());
    assertEquals("Text", stringResource.getName());
    assertEquals("Text", stringResource.getValue());
    byte[] byteArray = new byte[4];
    assertEquals(4, stringResource.getInputStream().read(byteArray));
    assertEquals(4L, stringResource.getSize());
    assertTrue(stringResource.isExists());
    assertArrayEquals("Text".getBytes("UTF-8"), byteArray);
  }

  /**
   * Test {@link StringResource#addText(String)}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringResource#addText(String)}
   */
  @Test
  public void testAddText_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new StringResource(new Project(), "42")).addText("Text"));
  }

  /**
   * Test {@link StringResource#addText(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringResource#addText(String)}
   */
  @Test
  public void testAddText_whenNull_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new StringResource(new Project(), "42")).addText(null));
  }

  /**
   * Test {@link StringResource#setEncoding(String)}.
   * <p>
   * Method under test: {@link StringResource#setEncoding(String)}
   */
  @Test
  public void testSetEncoding() {
    // Arrange
    StringResource stringResource = new StringResource();

    // Act
    stringResource.setEncoding("foo");

    // Assert
    assertEquals("foo", stringResource.getEncoding());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link StringResource#toString()}
   *   <li>{@link StringResource#getEncoding()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    StringResource stringResource = new StringResource();

    // Act
    String actualToStringResult = stringResource.toString();

    // Assert
    assertEquals("UTF-8", stringResource.getEncoding());
    assertEquals("null", actualToStringResult);
  }

  /**
   * Test {@link StringResource#getSize()}.
   * <ul>
   *   <li>Given {@link StringResource#StringResource(String)} with value is {@code 42}.</li>
   *   <li>Then return two.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringResource#getSize()}
   */
  @Test
  public void testGetSize_givenStringResourceWithValueIs42_thenReturnTwo() {
    // Arrange, Act and Assert
    assertEquals(2L, (new StringResource("42")).getSize());
  }

  /**
   * Test {@link Resource#equals(Object)}, and {@link StringResource#hashCode()}.
   * <ul>
   *   <li>When other is same.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link StringResource#equals(Object)}
   *   <li>{@link StringResource#hashCode()}
   * </ul>
   */
  @Test
  public void testEqualsAndHashCode_whenOtherIsSame_thenReturnEqual() {
    // Arrange
    StringResource stringResource = new StringResource();

    // Act and Assert
    assertEquals(stringResource, stringResource);
    int expectedHashCodeResult = stringResource.hashCode();
    assertEquals(expectedHashCodeResult, stringResource.hashCode());
  }

  /**
   * Test {@link Resource#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringResource#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(new StringResource(), 1);
  }

  /**
   * Test {@link Resource#equals(Object)}.
   * <ul>
   *   <li>When other is {@code null}.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringResource#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsNull_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(new StringResource(), null);
  }

  /**
   * Test {@link Resource#equals(Object)}.
   * <ul>
   *   <li>When other is wrong type.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringResource#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsWrongType_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(new StringResource(), "Different type to StringResource");
  }

  /**
   * Test {@link StringResource#getInputStream()}.
   * <ul>
   *   <li>Given {@link StringResource#StringResource()} Encoding is {@code null}.</li>
   *   <li>Then return read is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringResource#getInputStream()}
   */
  @Test
  public void testGetInputStream_givenStringResourceEncodingIsNull_thenReturnReadIsThree() throws IOException {
    // Arrange
    StringResource stringResource = new StringResource();
    stringResource.setEncoding(null);
    stringResource.setName("foo");

    // Act and Assert
    byte[] byteArray = new byte[3];
    assertEquals(3, stringResource.getInputStream().read(byteArray));
    assertArrayEquals("foo".getBytes("UTF-8"), byteArray);
  }

  /**
   * Test {@link StringResource#getInputStream()}.
   * <ul>
   *   <li>Given {@link StringResource#StringResource()} Name is {@code foo}.</li>
   *   <li>Then return read is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringResource#getInputStream()}
   */
  @Test
  public void testGetInputStream_givenStringResourceNameIsFoo_thenReturnReadIsThree() throws IOException {
    // Arrange
    StringResource stringResource = new StringResource();
    stringResource.setName("foo");

    // Act and Assert
    byte[] byteArray = new byte[3];
    assertEquals(3, stringResource.getInputStream().read(byteArray));
    assertArrayEquals("foo".getBytes("UTF-8"), byteArray);
  }

  /**
   * Test {@link StringResource#getInputStream()}.
   * <ul>
   *   <li>Given {@link StringResource#StringResource()}.</li>
   *   <li>Then throw {@link IllegalStateException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringResource#getInputStream()}
   */
  @Test
  public void testGetInputStream_givenStringResource_thenThrowIllegalStateException() throws IOException {
    // Arrange, Act and Assert
    assertThrows(IllegalStateException.class, () -> (new StringResource()).getInputStream());
  }

  /**
   * Test {@link StringResource#getOutputStream()}.
   * <ul>
   *   <li>Then throw {@link ImmutableResourceException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringResource#getOutputStream()}
   */
  @Test
  public void testGetOutputStream_thenThrowImmutableResourceException() throws IOException {
    // Arrange
    StringResource stringResource = new StringResource();
    stringResource.setName("foo");

    // Act and Assert
    assertThrows(ImmutableResourceException.class, () -> stringResource.getOutputStream());
  }

  /**
   * Test {@link StringResource#setRefid(Reference)}.
   * <ul>
   *   <li>Given {@link StringResource#StringResource()}.</li>
   *   <li>Then {@link StringResource#StringResource()} Reference.</li>
   * </ul>
   * <p>
   * Method under test: {@link StringResource#setRefid(Reference)}
   */
  @Test
  public void testSetRefid_givenStringResource_thenStringResourceReference() {
    // Arrange
    StringResource stringResource = new StringResource();
    Reference r = new Reference("42");

    // Act
    stringResource.setRefid(r);

    // Assert
    assertTrue(stringResource.isReference());
    assertSame(r, stringResource.getRefid());
  }

  /**
   * Test {@link StringResource#getContent()}.
   * <p>
   * Method under test: {@link StringResource#getContent()}
   */
  @Test
  public void testGetContent() {
    // Arrange, Act and Assert
    assertNull((new StringResource()).getContent());
  }
}
