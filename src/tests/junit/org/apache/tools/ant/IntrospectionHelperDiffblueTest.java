package org.apache.tools.ant;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.util.Map;
import org.apache.tools.ant.taskdefs.DynamicTask;
import org.apache.tools.ant.taskdefs.DynamicTask.Sub;
import org.junit.Test;

public class IntrospectionHelperDiffblueTest {
  /**
   * Test {@link IntrospectionHelper#getHelper(Class)} with {@code c}.
   * <ul>
   *   <li>When {@code Object}.</li>
   *   <li>Then return not Container.</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#getHelper(Class)}
   */
  @Test
  public void testGetHelperWithC_whenJavaLangObject_thenReturnNotContainer() {
    // Arrange
    Class<Object> c = Object.class;

    // Act
    IntrospectionHelper actualHelper = IntrospectionHelper.getHelper(c);

    // Assert
    assertFalse(actualHelper.isContainer());
    assertFalse(actualHelper.isDynamic());
    assertTrue(actualHelper.getExtensionPoints().isEmpty());
    Map<String, Class<?>> attributeMap = actualHelper.getAttributeMap();
    assertTrue(attributeMap.isEmpty());
    assertSame(attributeMap, actualHelper.getNestedElementMap());
  }

  /**
   * Test {@link IntrospectionHelper#getHelper(Project, Class)} with {@code p}, {@code c}.
   * <ul>
   *   <li>When {@code Object}.</li>
   *   <li>Then return not Container.</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#getHelper(Project, Class)}
   */
  @Test
  public void testGetHelperWithPC_whenJavaLangObject_thenReturnNotContainer() {
    // Arrange
    Class<Object> c = Object.class;

    // Act
    IntrospectionHelper actualHelper = IntrospectionHelper.getHelper(null, c);

    // Assert
    assertFalse(actualHelper.isContainer());
    assertFalse(actualHelper.isDynamic());
    assertTrue(actualHelper.getExtensionPoints().isEmpty());
    Map<String, Class<?>> attributeMap = actualHelper.getAttributeMap();
    assertTrue(attributeMap.isEmpty());
    assertSame(attributeMap, actualHelper.getNestedElementMap());
  }

  /**
   * Test {@link IntrospectionHelper#getHelper(Project, Class)} with {@code p}, {@code c}.
   * <ul>
   *   <li>When {@link Project} (default constructor).</li>
   *   <li>Then return not Container.</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#getHelper(Project, Class)}
   */
  @Test
  public void testGetHelperWithPC_whenProject_thenReturnNotContainer() {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;

    // Act
    IntrospectionHelper actualHelper = IntrospectionHelper.getHelper(p, c);

    // Assert
    assertFalse(actualHelper.isContainer());
    assertFalse(actualHelper.isDynamic());
    assertTrue(actualHelper.getExtensionPoints().isEmpty());
    Map<String, Class<?>> attributeMap = actualHelper.getAttributeMap();
    assertTrue(attributeMap.isEmpty());
    assertSame(attributeMap, actualHelper.getNestedElementMap());
  }

  /**
   * Test {@link IntrospectionHelper#setAttribute(Project, Object, String, Object)} with {@code Project}, {@code Object}, {@code String}, {@code Object}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#setAttribute(Project, Object, String, Object)}
   */
  @Test
  public void testSetAttributeWithProjectObjectStringObject_givenAntClassLoader() throws BuildException {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;
    IntrospectionHelper helper = IntrospectionHelper.getHelper(p, c);

    Project p2 = new Project();
    p2.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(UnsupportedAttributeException.class,
        () -> helper.setAttribute(p2, "Element", "Attribute Name", (Object) "Value"));
  }

  /**
   * Test {@link IntrospectionHelper#setAttribute(Project, Object, String, Object)} with {@code Project}, {@code Object}, {@code String}, {@code Object}.
   * <ul>
   *   <li>Given {@code :}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#setAttribute(Project, Object, String, Object)}
   */
  @Test
  public void testSetAttributeWithProjectObjectStringObject_givenColon() throws BuildException {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;
    IntrospectionHelper helper = IntrospectionHelper.getHelper(p, c);

    Project p2 = new Project();
    p2.setDefault(":");
    p2.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(UnsupportedAttributeException.class,
        () -> helper.setAttribute(p2, "Element", "Attribute Name", (Object) "Value"));
  }

  /**
   * Test {@link IntrospectionHelper#setAttribute(Project, Object, String, Object)} with {@code Project}, {@code Object}, {@code String}, {@code Object}.
   * <ul>
   *   <li>Given {@link ComponentHelper#COMPONENT_HELPER_REFERENCE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#setAttribute(Project, Object, String, Object)}
   */
  @Test
  public void testSetAttributeWithProjectObjectStringObject_givenComponent_helper_reference() throws BuildException {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;
    IntrospectionHelper helper = IntrospectionHelper.getHelper(p, c);

    Project p2 = new Project();
    Class<Object> typeClass = Object.class;
    p2.addDataTypeDefinition(ComponentHelper.COMPONENT_HELPER_REFERENCE, typeClass);
    p2.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(UnsupportedAttributeException.class,
        () -> helper.setAttribute(p2, "Element", "Attribute Name", (Object) "Value"));
  }

  /**
   * Test {@link IntrospectionHelper#setAttribute(Project, Object, String, Object)} with {@code Project}, {@code Object}, {@code String}, {@code Object}.
   * <ul>
   *   <li>When {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#setAttribute(Project, Object, String, Object)}
   */
  @Test
  public void testSetAttributeWithProjectObjectStringObject_whenProject() throws BuildException {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;
    IntrospectionHelper helper = IntrospectionHelper.getHelper(p, c);

    // Act and Assert
    assertThrows(UnsupportedAttributeException.class,
        () -> helper.setAttribute(new Project(), "Element", "Attribute Name", (Object) "Value"));
  }

  /**
   * Test {@link IntrospectionHelper#setAttribute(Project, Object, String, String)} with {@code Project}, {@code Object}, {@code String}, {@code String}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#setAttribute(Project, Object, String, String)}
   */
  @Test
  public void testSetAttributeWithProjectObjectStringString_givenAntClassLoader() throws BuildException {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;
    IntrospectionHelper helper = IntrospectionHelper.getHelper(p, c);

    Project p2 = new Project();
    p2.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(UnsupportedAttributeException.class, () -> helper.setAttribute(p2, "Element", "Attribute Name", "42"));
  }

  /**
   * Test {@link IntrospectionHelper#setAttribute(Project, Object, String, String)} with {@code Project}, {@code Object}, {@code String}, {@code String}.
   * <ul>
   *   <li>Given {@code :}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#setAttribute(Project, Object, String, String)}
   */
  @Test
  public void testSetAttributeWithProjectObjectStringString_givenColon() throws BuildException {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;
    IntrospectionHelper helper = IntrospectionHelper.getHelper(p, c);

    Project p2 = new Project();
    p2.setDefault(":");
    p2.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(UnsupportedAttributeException.class, () -> helper.setAttribute(p2, "Element", "Attribute Name", "42"));
  }

  /**
   * Test {@link IntrospectionHelper#setAttribute(Project, Object, String, String)} with {@code Project}, {@code Object}, {@code String}, {@code String}.
   * <ul>
   *   <li>Given {@link ComponentHelper#COMPONENT_HELPER_REFERENCE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#setAttribute(Project, Object, String, String)}
   */
  @Test
  public void testSetAttributeWithProjectObjectStringString_givenComponent_helper_reference() throws BuildException {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;
    IntrospectionHelper helper = IntrospectionHelper.getHelper(p, c);

    Project p2 = new Project();
    Class<Object> typeClass = Object.class;
    p2.addDataTypeDefinition(ComponentHelper.COMPONENT_HELPER_REFERENCE, typeClass);
    p2.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(UnsupportedAttributeException.class, () -> helper.setAttribute(p2, "Element", "Attribute Name", "42"));
  }

  /**
   * Test {@link IntrospectionHelper#setAttribute(Project, Object, String, String)} with {@code Project}, {@code Object}, {@code String}, {@code String}.
   * <ul>
   *   <li>When {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#setAttribute(Project, Object, String, String)}
   */
  @Test
  public void testSetAttributeWithProjectObjectStringString_whenProject() throws BuildException {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;
    IntrospectionHelper helper = IntrospectionHelper.getHelper(p, c);

    // Act and Assert
    assertThrows(UnsupportedAttributeException.class,
        () -> helper.setAttribute(new Project(), "Element", "Attribute Name", "42"));
  }

  /**
   * Test {@link IntrospectionHelper#addText(Project, Object, String)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>When {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#addText(Project, Object, String)}
   */
  @Test
  public void testAddText_givenAntClassLoader_whenProjectAddBuildListenerAntClassLoader() throws BuildException {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;
    IntrospectionHelper helper = IntrospectionHelper.getHelper(p, c);

    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(BuildException.class, () -> helper.addText(project, "Element", "Text"));
  }

  /**
   * Test {@link IntrospectionHelper#addText(Project, Object, String)}.
   * <ul>
   *   <li>Given {@link ComponentHelper#COMPONENT_HELPER_REFERENCE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#addText(Project, Object, String)}
   */
  @Test
  public void testAddText_givenComponent_helper_reference() throws BuildException {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;
    IntrospectionHelper helper = IntrospectionHelper.getHelper(p, c);

    Project project = new Project();
    project.setDefault(ComponentHelper.COMPONENT_HELPER_REFERENCE);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(BuildException.class, () -> helper.addText(project, "Element", "Text"));
  }

  /**
   * Test {@link IntrospectionHelper#addText(Project, Object, String)}.
   * <ul>
   *   <li>Given {@link Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#addText(Project, Object, String)}
   */
  @Test
  public void testAddText_givenObject() throws BuildException {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;
    IntrospectionHelper helper = IntrospectionHelper.getHelper(p, c);

    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition(MagicNames.REFID_PROPERTY_HELPER, typeClass);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(BuildException.class, () -> helper.addText(project, "Element", "Text"));
  }

  /**
   * Test {@link IntrospectionHelper#addText(Project, Object, String)}.
   * <ul>
   *   <li>Given {@code Value}.</li>
   *   <li>When {@link Project} (default constructor) addReference {@link MagicNames#REFID_PROPERTY_HELPER} and {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#addText(Project, Object, String)}
   */
  @Test
  public void testAddText_givenValue_whenProjectAddReferenceRefid_property_helperAndValue() throws BuildException {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;
    IntrospectionHelper helper = IntrospectionHelper.getHelper(p, c);

    Project project = new Project();
    project.addReference(MagicNames.REFID_PROPERTY_HELPER, "Value");
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(BuildException.class, () -> helper.addText(project, "Element", "Text"));
  }

  /**
   * Test {@link IntrospectionHelper#addText(Project, Object, String)}.
   * <ul>
   *   <li>When {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#addText(Project, Object, String)}
   */
  @Test
  public void testAddText_whenProject_thenThrowBuildException() throws BuildException {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;
    IntrospectionHelper helper = IntrospectionHelper.getHelper(p, c);

    // Act and Assert
    assertThrows(BuildException.class, () -> helper.addText(new Project(), "Element", "Text"));
  }

  /**
   * Test {@link IntrospectionHelper#throwNotSupported(Project, Object, String)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#throwNotSupported(Project, Object, String)}
   */
  @Test
  public void testThrowNotSupported_givenAntClassLoader() {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;
    IntrospectionHelper helper = IntrospectionHelper.getHelper(p, c);

    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(UnsupportedElementException.class, () -> helper.throwNotSupported(project, "Parent", "Element Name"));
  }

  /**
   * Test {@link IntrospectionHelper#throwNotSupported(Project, Object, String)}.
   * <ul>
   *   <li>Given {@link ComponentHelper#COMPONENT_HELPER_REFERENCE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#throwNotSupported(Project, Object, String)}
   */
  @Test
  public void testThrowNotSupported_givenComponent_helper_reference() {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;
    IntrospectionHelper helper = IntrospectionHelper.getHelper(p, c);

    Project project = new Project();
    project.setDefault(ComponentHelper.COMPONENT_HELPER_REFERENCE);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(UnsupportedElementException.class, () -> helper.throwNotSupported(project, "Parent", "Element Name"));
  }

  /**
   * Test {@link IntrospectionHelper#throwNotSupported(Project, Object, String)}.
   * <ul>
   *   <li>Given {@link Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#throwNotSupported(Project, Object, String)}
   */
  @Test
  public void testThrowNotSupported_givenObject() {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;
    IntrospectionHelper helper = IntrospectionHelper.getHelper(p, c);

    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition(MagicNames.REFID_PROPERTY_HELPER, typeClass);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(UnsupportedElementException.class, () -> helper.throwNotSupported(project, "Parent", "Element Name"));
  }

  /**
   * Test {@link IntrospectionHelper#throwNotSupported(Project, Object, String)}.
   * <ul>
   *   <li>Given {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#throwNotSupported(Project, Object, String)}
   */
  @Test
  public void testThrowNotSupported_givenValue() {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;
    IntrospectionHelper helper = IntrospectionHelper.getHelper(p, c);

    Project project = new Project();
    project.addReference(MagicNames.REFID_PROPERTY_HELPER, "Value");
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(UnsupportedElementException.class, () -> helper.throwNotSupported(project, "Parent", "Element Name"));
  }

  /**
   * Test {@link IntrospectionHelper#throwNotSupported(Project, Object, String)}.
   * <ul>
   *   <li>When {@link Project} (default constructor).</li>
   *   <li>Then throw {@link UnsupportedElementException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#throwNotSupported(Project, Object, String)}
   */
  @Test
  public void testThrowNotSupported_whenProject_thenThrowUnsupportedElementException() {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;
    IntrospectionHelper helper = IntrospectionHelper.getHelper(p, c);

    // Act and Assert
    assertThrows(UnsupportedElementException.class,
        () -> helper.throwNotSupported(new Project(), "Parent", "Element Name"));
  }

  /**
   * Test {@link IntrospectionHelper#createElement(Project, Object, String)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>When {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#createElement(Project, Object, String)}
   */
  @Test
  public void testCreateElement_givenAntClassLoader_whenProjectAddBuildListenerAntClassLoader() throws BuildException {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;
    IntrospectionHelper helper = IntrospectionHelper.getHelper(p, c);

    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(UnsupportedElementException.class, () -> helper.createElement(project, "Parent", "Element Name"));
  }

  /**
   * Test {@link IntrospectionHelper#createElement(Project, Object, String)}.
   * <ul>
   *   <li>Given {@link ProjectHelper#ANT_CORE_URI}.</li>
   *   <li>When {@link Project} (default constructor) Default is {@link ProjectHelper#ANT_CORE_URI}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#createElement(Project, Object, String)}
   */
  @Test
  public void testCreateElement_givenAnt_core_uri_whenProjectDefaultIsAnt_core_uri() throws BuildException {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;
    IntrospectionHelper helper = IntrospectionHelper.getHelper(p, c);

    Project project = new Project();
    project.setDefault(ProjectHelper.ANT_CORE_URI);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(UnsupportedElementException.class, () -> helper.createElement(project, "Parent", "Element Name"));
  }

  /**
   * Test {@link IntrospectionHelper#createElement(Project, Object, String)}.
   * <ul>
   *   <li>Given {@link ComponentHelper#COMPONENT_HELPER_REFERENCE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#createElement(Project, Object, String)}
   */
  @Test
  public void testCreateElement_givenComponent_helper_reference() throws BuildException {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;
    IntrospectionHelper helper = IntrospectionHelper.getHelper(p, c);

    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition(ComponentHelper.COMPONENT_HELPER_REFERENCE, typeClass);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(UnsupportedElementException.class, () -> helper.createElement(project, "Parent", "Element Name"));
  }

  /**
   * Test {@link IntrospectionHelper#createElement(Project, Object, String)}.
   * <ul>
   *   <li>When {@link Project} (default constructor).</li>
   *   <li>Then throw {@link UnsupportedElementException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#createElement(Project, Object, String)}
   */
  @Test
  public void testCreateElement_whenProject_thenThrowUnsupportedElementException() throws BuildException {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;
    IntrospectionHelper helper = IntrospectionHelper.getHelper(p, c);

    // Act and Assert
    assertThrows(UnsupportedElementException.class,
        () -> helper.createElement(new Project(), "Parent", "Element Name"));
  }

  /**
   * Test {@link IntrospectionHelper#getElementCreator(Project, String, Object, String, UnknownElement)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#getElementCreator(Project, String, Object, String, UnknownElement)}
   */
  @Test
  public void testGetElementCreator_givenAntClassLoader() {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;
    IntrospectionHelper helper = IntrospectionHelper.getHelper(p, c);

    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(UnsupportedElementException.class, () -> helper.getElementCreator(project, "Parent Uri", "Parent",
        "Element Name", new UnknownElement("Element Name")));
  }

  /**
   * Test {@link IntrospectionHelper#getElementCreator(Project, String, Object, String, UnknownElement)}.
   * <ul>
   *   <li>Given {@link ProjectHelper#ANT_CORE_URI}.</li>
   *   <li>When {@link Project} (default constructor) Default is {@link ProjectHelper#ANT_CORE_URI}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#getElementCreator(Project, String, Object, String, UnknownElement)}
   */
  @Test
  public void testGetElementCreator_givenAnt_core_uri_whenProjectDefaultIsAnt_core_uri() {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;
    IntrospectionHelper helper = IntrospectionHelper.getHelper(p, c);

    Project project = new Project();
    project.setDefault(ProjectHelper.ANT_CORE_URI);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(UnsupportedElementException.class, () -> helper.getElementCreator(project, "Parent Uri", "Parent",
        "Element Name", new UnknownElement("Element Name")));
  }

  /**
   * Test {@link IntrospectionHelper#getElementCreator(Project, String, Object, String, UnknownElement)}.
   * <ul>
   *   <li>Given {@link DefaultLogger} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#getElementCreator(Project, String, Object, String, UnknownElement)}
   */
  @Test
  public void testGetElementCreator_givenDefaultLogger() {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;
    IntrospectionHelper helper = IntrospectionHelper.getHelper(p, c);

    Project project = new Project();
    project.addBuildListener(new DefaultLogger());

    // Act and Assert
    assertThrows(UnsupportedElementException.class, () -> helper.getElementCreator(project, "Parent Uri", "Parent",
        "Element Name", new UnknownElement("Element Name")));
  }

  /**
   * Test {@link IntrospectionHelper#getElementCreator(Project, String, Object, String, UnknownElement)}.
   * <ul>
   *   <li>Given {@code String}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#getElementCreator(Project, String, Object, String, UnknownElement)}
   */
  @Test
  public void testGetElementCreator_givenJavaLangString() {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;
    IntrospectionHelper helper = IntrospectionHelper.getHelper(p, c);

    Project project = new Project();
    Class<String> typeClass = String.class;
    project.addDataTypeDefinition("Parent Uri", typeClass);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(UnsupportedElementException.class, () -> helper.getElementCreator(project, "Parent Uri", "Parent",
        "Element Name", new UnknownElement("Element Name")));
  }

  /**
   * Test {@link IntrospectionHelper#getElementCreator(Project, String, Object, String, UnknownElement)}.
   * <ul>
   *   <li>Given {@link Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#getElementCreator(Project, String, Object, String, UnknownElement)}
   */
  @Test
  public void testGetElementCreator_givenObject() {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;
    IntrospectionHelper helper = IntrospectionHelper.getHelper(p, c);

    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Parent Uri", typeClass);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(UnsupportedElementException.class, () -> helper.getElementCreator(project, "Parent Uri", "Parent",
        "Element Name", new UnknownElement("Element Name")));
  }

  /**
   * Test {@link IntrospectionHelper#getElementCreator(Project, String, Object, String, UnknownElement)}.
   * <ul>
   *   <li>When {@link ProjectHelper#ANT_CORE_URI}.</li>
   *   <li>Then throw {@link UnsupportedElementException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#getElementCreator(Project, String, Object, String, UnknownElement)}
   */
  @Test
  public void testGetElementCreator_whenAnt_core_uri_thenThrowUnsupportedElementException() {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;
    IntrospectionHelper helper = IntrospectionHelper.getHelper(p, c);
    Project project = new Project();

    // Act and Assert
    assertThrows(UnsupportedElementException.class, () -> helper.getElementCreator(project, ProjectHelper.ANT_CORE_URI,
        "Parent", "Element Name", new UnknownElement("Element Name")));
  }

  /**
   * Test {@link IntrospectionHelper#getElementCreator(Project, String, Object, String, UnknownElement)}.
   * <ul>
   *   <li>When {@link ProjectHelper#ANT_CORE_URI}.</li>
   *   <li>Then throw {@link UnsupportedElementException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#getElementCreator(Project, String, Object, String, UnknownElement)}
   */
  @Test
  public void testGetElementCreator_whenAnt_core_uri_thenThrowUnsupportedElementException2() {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;
    IntrospectionHelper helper = IntrospectionHelper.getHelper(p, c);
    Project project = new Project();

    // Act and Assert
    assertThrows(UnsupportedElementException.class, () -> helper.getElementCreator(project, "Parent Uri", "Parent",
        ProjectHelper.ANT_CORE_URI, new UnknownElement("Element Name")));
  }

  /**
   * Test {@link IntrospectionHelper#getElementCreator(Project, String, Object, String, UnknownElement)}.
   * <ul>
   *   <li>When {@link DynamicTask} (default constructor).</li>
   *   <li>Then return RealObject is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#getElementCreator(Project, String, Object, String, UnknownElement)}
   */
  @Test
  public void testGetElementCreator_whenDynamicTask_thenReturnRealObjectIsNull() {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;
    IntrospectionHelper helper = IntrospectionHelper.getHelper(p, c);
    Project project = new Project();
    DynamicTask dynamicTask = new DynamicTask();

    // Act and Assert
    assertNull(
        helper.getElementCreator(project, "Parent Uri", dynamicTask, "Element Name", new UnknownElement("Element Name"))
            .getRealObject());
  }

  /**
   * Test {@link IntrospectionHelper#getElementCreator(Project, String, Object, String, UnknownElement)}.
   * <ul>
   *   <li>When {@link Project} (default constructor).</li>
   *   <li>Then throw {@link UnsupportedElementException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#getElementCreator(Project, String, Object, String, UnknownElement)}
   */
  @Test
  public void testGetElementCreator_whenProject_thenThrowUnsupportedElementException() {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;
    IntrospectionHelper helper = IntrospectionHelper.getHelper(p, c);
    Project project = new Project();

    // Act and Assert
    assertThrows(UnsupportedElementException.class, () -> helper.getElementCreator(project, "Parent Uri", "Parent",
        "Element Name", new UnknownElement("Element Name")));
  }

  /**
   * Test {@link IntrospectionHelper#getElementCreator(Project, String, Object, String, UnknownElement)}.
   * <ul>
   *   <li>When {@link Sub#Sub(DynamicTask)} with this$0 is {@link DynamicTask} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#getElementCreator(Project, String, Object, String, UnknownElement)}
   */
  @Test
  public void testGetElementCreator_whenSubWithThis$0IsDynamicTask() {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;
    IntrospectionHelper helper = IntrospectionHelper.getHelper(p, c);
    Project project = new Project();
    Sub sub = (new DynamicTask()).new Sub();

    // Act and Assert
    assertThrows(UnsupportedElementException.class,
        () -> helper.getElementCreator(project, "Parent Uri", sub, "Element Name", new UnknownElement("Element Name")));
  }

  /**
   * Test {@link IntrospectionHelper#isDynamic()}.
   * <p>
   * Method under test: {@link IntrospectionHelper#isDynamic()}
   */
  @Test
  public void testIsDynamic() {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;

    // Act and Assert
    assertFalse(IntrospectionHelper.getHelper(p, c).isDynamic());
  }

  /**
   * Test {@link IntrospectionHelper#isContainer()}.
   * <p>
   * Method under test: {@link IntrospectionHelper#isContainer()}
   */
  @Test
  public void testIsContainer() {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;

    // Act and Assert
    assertFalse(IntrospectionHelper.getHelper(p, c).isContainer());
  }

  /**
   * Test {@link IntrospectionHelper#supportsNestedElement(String)} with {@code elementName}.
   * <ul>
   *   <li>When {@code :}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#supportsNestedElement(String)}
   */
  @Test
  public void testSupportsNestedElementWithElementName_whenColon() {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;

    // Act and Assert
    assertFalse(IntrospectionHelper.getHelper(p, c).supportsNestedElement(":"));
  }

  /**
   * Test {@link IntrospectionHelper#supportsNestedElement(String)} with {@code elementName}.
   * <ul>
   *   <li>When {@code Element Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#supportsNestedElement(String)}
   */
  @Test
  public void testSupportsNestedElementWithElementName_whenElementName() {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;

    // Act and Assert
    assertFalse(IntrospectionHelper.getHelper(p, c).supportsNestedElement("Element Name"));
  }

  /**
   * Test {@link IntrospectionHelper#supportsNestedElement(String, String, Project, Object)} with {@code parentUri}, {@code elementName}, {@code project}, {@code parent}.
   * <ul>
   *   <li>When {@code :}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#supportsNestedElement(String, String, Project, Object)}
   */
  @Test
  public void testSupportsNestedElementWithParentUriElementNameProjectParent_whenColon() {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;
    IntrospectionHelper helper = IntrospectionHelper.getHelper(p, c);

    // Act and Assert
    assertFalse(helper.supportsNestedElement("Parent Uri", ":", new Project(), "Parent"));
  }

  /**
   * Test {@link IntrospectionHelper#supportsNestedElement(String, String, Project, Object)} with {@code parentUri}, {@code elementName}, {@code project}, {@code parent}.
   * <ul>
   *   <li>When {@code Element Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#supportsNestedElement(String, String, Project, Object)}
   */
  @Test
  public void testSupportsNestedElementWithParentUriElementNameProjectParent_whenElementName() {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;
    IntrospectionHelper helper = IntrospectionHelper.getHelper(p, c);

    // Act and Assert
    assertFalse(helper.supportsNestedElement("Parent Uri", "Element Name", new Project(), "Parent"));
  }

  /**
   * Test {@link IntrospectionHelper#supportsNestedElement(String, String)} with {@code parentUri}, {@code elementName}.
   * <ul>
   *   <li>When {@code :}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#supportsNestedElement(String, String)}
   */
  @Test
  public void testSupportsNestedElementWithParentUriElementName_whenColon() {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;

    // Act and Assert
    assertFalse(IntrospectionHelper.getHelper(p, c).supportsNestedElement("Parent Uri", ":"));
  }

  /**
   * Test {@link IntrospectionHelper#supportsNestedElement(String, String)} with {@code parentUri}, {@code elementName}.
   * <ul>
   *   <li>When {@code Element Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#supportsNestedElement(String, String)}
   */
  @Test
  public void testSupportsNestedElementWithParentUriElementName_whenElementName() {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;

    // Act and Assert
    assertFalse(IntrospectionHelper.getHelper(p, c).supportsNestedElement("Parent Uri", "Element Name"));
  }

  /**
   * Test {@link IntrospectionHelper#supportsReflectElement(String, String)}.
   * <ul>
   *   <li>When {@code :}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#supportsReflectElement(String, String)}
   */
  @Test
  public void testSupportsReflectElement_whenColon() {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;

    // Act and Assert
    assertFalse(IntrospectionHelper.getHelper(p, c).supportsReflectElement("Parent Uri", ":"));
  }

  /**
   * Test {@link IntrospectionHelper#supportsReflectElement(String, String)}.
   * <ul>
   *   <li>When {@code Element Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#supportsReflectElement(String, String)}
   */
  @Test
  public void testSupportsReflectElement_whenElementName() {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;

    // Act and Assert
    assertFalse(IntrospectionHelper.getHelper(p, c).supportsReflectElement("Parent Uri", "Element Name"));
  }

  /**
   * Test {@link IntrospectionHelper#getElementType(String)}.
   * <ul>
   *   <li>When {@code Element Name}.</li>
   *   <li>Then throw {@link UnsupportedElementException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#getElementType(String)}
   */
  @Test
  public void testGetElementType_whenElementName_thenThrowUnsupportedElementException() throws BuildException {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;

    // Act and Assert
    assertThrows(UnsupportedElementException.class,
        () -> IntrospectionHelper.getHelper(p, c).getElementType("Element Name"));
  }

  /**
   * Test {@link IntrospectionHelper#getAttributeType(String)}.
   * <ul>
   *   <li>When {@code Attribute Name}.</li>
   *   <li>Then throw {@link UnsupportedAttributeException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#getAttributeType(String)}
   */
  @Test
  public void testGetAttributeType_whenAttributeName_thenThrowUnsupportedAttributeException() throws BuildException {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;

    // Act and Assert
    assertThrows(UnsupportedAttributeException.class,
        () -> IntrospectionHelper.getHelper(p, c).getAttributeType("Attribute Name"));
  }

  /**
   * Test {@link IntrospectionHelper#getAddTextMethod()}.
   * <p>
   * Method under test: {@link IntrospectionHelper#getAddTextMethod()}
   */
  @Test
  public void testGetAddTextMethod() throws BuildException {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;

    // Act and Assert
    assertThrows(BuildException.class, () -> IntrospectionHelper.getHelper(p, c).getAddTextMethod());
  }

  /**
   * Test {@link IntrospectionHelper#getElementMethod(String)}.
   * <ul>
   *   <li>When {@code Element Name}.</li>
   *   <li>Then throw {@link UnsupportedElementException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#getElementMethod(String)}
   */
  @Test
  public void testGetElementMethod_whenElementName_thenThrowUnsupportedElementException() throws BuildException {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;

    // Act and Assert
    assertThrows(UnsupportedElementException.class,
        () -> IntrospectionHelper.getHelper(p, c).getElementMethod("Element Name"));
  }

  /**
   * Test {@link IntrospectionHelper#getAttributeMethod(String)}.
   * <ul>
   *   <li>When {@code Attribute Name}.</li>
   *   <li>Then throw {@link UnsupportedAttributeException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IntrospectionHelper#getAttributeMethod(String)}
   */
  @Test
  public void testGetAttributeMethod_whenAttributeName_thenThrowUnsupportedAttributeException() throws BuildException {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;

    // Act and Assert
    assertThrows(UnsupportedAttributeException.class,
        () -> IntrospectionHelper.getHelper(p, c).getAttributeMethod("Attribute Name"));
  }

  /**
   * Test {@link IntrospectionHelper#supportsCharacters()}.
   * <p>
   * Method under test: {@link IntrospectionHelper#supportsCharacters()}
   */
  @Test
  public void testSupportsCharacters() {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;

    // Act and Assert
    assertFalse(IntrospectionHelper.getHelper(p, c).supportsCharacters());
  }

  /**
   * Test {@link IntrospectionHelper#getAttributeMap()}.
   * <p>
   * Method under test: {@link IntrospectionHelper#getAttributeMap()}
   */
  @Test
  public void testGetAttributeMap() {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;

    // Act and Assert
    assertTrue(IntrospectionHelper.getHelper(p, c).getAttributeMap().isEmpty());
  }

  /**
   * Test {@link IntrospectionHelper#getNestedElementMap()}.
   * <p>
   * Method under test: {@link IntrospectionHelper#getNestedElementMap()}
   */
  @Test
  public void testGetNestedElementMap() {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;

    // Act and Assert
    assertTrue(IntrospectionHelper.getHelper(p, c).getNestedElementMap().isEmpty());
  }

  /**
   * Test {@link IntrospectionHelper#getExtensionPoints()}.
   * <p>
   * Method under test: {@link IntrospectionHelper#getExtensionPoints()}
   */
  @Test
  public void testGetExtensionPoints() {
    // Arrange
    Project p = new Project();
    Class<Object> c = Object.class;

    // Act and Assert
    assertTrue(IntrospectionHelper.getHelper(p, c).getExtensionPoints().isEmpty());
  }
}
