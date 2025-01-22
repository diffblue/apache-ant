package org.apache.tools.ant;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.util.Collection;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.Vector;
import org.apache.tools.ant.PropertyHelper.Delegate;
import org.apache.tools.ant.PropertyHelper.PropertyEvaluator;
import org.apache.tools.ant.property.LocalProperties;
import org.apache.tools.ant.property.PropertyExpander;
import org.apache.tools.ant.taskdefs.Execute;
import org.junit.Test;

public class PropertyHelperDiffblueTest {
  /**
   * Test {@link PropertyHelper#PropertyHelper()}.
   * <p>
   * Method under test: default or parameterless constructor of {@link PropertyHelper}
   */
  @Test
  public void testNewPropertyHelper() {
    // Arrange and Act
    PropertyHelper actualPropertyHelper = new PropertyHelper();

    // Assert
    Collection<PropertyExpander> expanders = actualPropertyHelper.getExpanders();
    assertEquals(2, expanders.size());
    assertTrue(expanders instanceof List);
    assertNull(actualPropertyHelper.getProject());
    assertNull(actualPropertyHelper.getNext());
    Hashtable<String, Object> inheritedProperties = actualPropertyHelper.getInheritedProperties();
    assertTrue(inheritedProperties.isEmpty());
    assertTrue(actualPropertyHelper.getPropertyNames().isEmpty());
    assertEquals(inheritedProperties, actualPropertyHelper.getInternalInheritedProperties());
    assertEquals(inheritedProperties, actualPropertyHelper.getInternalProperties());
    assertEquals(inheritedProperties, actualPropertyHelper.getInternalUserProperties());
    assertEquals(inheritedProperties, actualPropertyHelper.getProperties());
    assertEquals(inheritedProperties, actualPropertyHelper.getUserProperties());
  }

  /**
   * Test {@link PropertyHelper#getProperty(Project, String)} with {@code project}, {@code name}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#getProperty(Project, String)}
   */
  @Test
  public void testGetPropertyWithProjectName_givenAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertNull(PropertyHelper.getProperty(project, "Name"));
  }

  /**
   * Test {@link PropertyHelper#getProperty(Project, String)} with {@code project}, {@code name}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#getProperty(Project, String)}
   */
  @Test
  public void testGetPropertyWithProjectName_givenJavaLangObject() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Adding reference: ", typeClass);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertNull(PropertyHelper.getProperty(project, "Name"));
  }

  /**
   * Test {@link PropertyHelper#getProperty(Project, String)} with {@code project}, {@code name}.
   * <ul>
   *   <li>Given {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#getProperty(Project, String)}
   */
  @Test
  public void testGetPropertyWithProjectName_givenTarget() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("Adding reference: ", new Target());
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertNull(PropertyHelper.getProperty(project, "Name"));
  }

  /**
   * Test {@link PropertyHelper#getProperty(Project, String)} with {@code project}, {@code name}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#getProperty(Project, String)}
   */
  @Test
  public void testGetPropertyWithProjectName_whenNull() {
    // Arrange, Act and Assert
    assertNull(PropertyHelper.getProperty((Project) null, "Name"));
  }

  /**
   * Test {@link PropertyHelper#getProperty(Project, String)} with {@code project}, {@code name}.
   * <ul>
   *   <li>When {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#getProperty(Project, String)}
   */
  @Test
  public void testGetPropertyWithProjectName_whenProject() {
    // Arrange, Act and Assert
    assertNull(PropertyHelper.getProperty(new Project(), "Name"));
  }

  /**
   * Test {@link PropertyHelper#setProperty(String, Object, boolean)} with {@code name}, {@code value}, {@code verbose}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#setProperty(String, Object, boolean)}
   */
  @Test
  public void testSetPropertyWithNameValueVerbose_givenJavaLangObject() {
    // Arrange
    Project p = new Project();
    Class<Object> typeClass = Object.class;
    p.addDataTypeDefinition(" -> ", typeClass);
    p.addBuildListener(new AntClassLoader());

    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.setProject(p);
    propertyHelper.add(LocalProperties.get(new Project()));

    // Act
    propertyHelper.setProperty("Name", "Value", true);

    // Assert
    Hashtable<String, Object> internalProperties = propertyHelper.getInternalProperties();
    assertEquals(1, internalProperties.size());
    assertEquals("Value", internalProperties.get("Name"));
    Set<String> propertyNames = propertyHelper.getPropertyNames();
    assertEquals(1, propertyNames.size());
    assertTrue(propertyNames.contains("Name"));
    assertEquals(internalProperties, propertyHelper.getProperties());
  }

  /**
   * Test {@link PropertyHelper#setProperty(String, Object, boolean)} with {@code name}, {@code value}, {@code verbose}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#setProperty(String, Object, boolean)}
   */
  @Test
  public void testSetPropertyWithNameValueVerbose_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project p = new Project();
    p.addBuildListener(new AntClassLoader());

    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.setProject(p);
    propertyHelper.add(LocalProperties.get(new Project()));

    // Act
    propertyHelper.setProperty("Name", "Value", true);

    // Assert
    Hashtable<String, Object> internalProperties = propertyHelper.getInternalProperties();
    assertEquals(1, internalProperties.size());
    assertEquals("Value", internalProperties.get("Name"));
    Set<String> propertyNames = propertyHelper.getPropertyNames();
    assertEquals(1, propertyNames.size());
    assertTrue(propertyNames.contains("Name"));
    assertEquals(internalProperties, propertyHelper.getProperties());
  }

  /**
   * Test {@link PropertyHelper#setProperty(String, Object, boolean)} with {@code name}, {@code value}, {@code verbose}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#setProperty(String, Object, boolean)}
   */
  @Test
  public void testSetPropertyWithNameValueVerbose_givenPropertyHelper() {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();

    // Act
    propertyHelper.setProperty("Name", "Value", true);

    // Assert
    Hashtable<String, Object> internalProperties = propertyHelper.getInternalProperties();
    assertEquals(1, internalProperties.size());
    assertEquals("Value", internalProperties.get("Name"));
    Set<String> propertyNames = propertyHelper.getPropertyNames();
    assertEquals(1, propertyNames.size());
    assertTrue(propertyNames.contains("Name"));
    assertEquals(internalProperties, propertyHelper.getProperties());
  }

  /**
   * Test {@link PropertyHelper#setProperty(String, Object, boolean)} with {@code name}, {@code value}, {@code verbose}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()} add {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#setProperty(String, Object, boolean)}
   */
  @Test
  public void testSetPropertyWithNameValueVerbose_givenPropertyHelperAddProject() {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.add(LocalProperties.get(new Project()));

    // Act
    propertyHelper.setProperty("Name", "Value", true);

    // Assert
    Hashtable<String, Object> internalProperties = propertyHelper.getInternalProperties();
    assertEquals(1, internalProperties.size());
    assertEquals("Value", internalProperties.get("Name"));
    Set<String> propertyNames = propertyHelper.getPropertyNames();
    assertEquals(1, propertyNames.size());
    assertTrue(propertyNames.contains("Name"));
    assertEquals(internalProperties, propertyHelper.getProperties());
  }

  /**
   * Test {@link PropertyHelper#setProperty(String, Object, boolean)} with {@code name}, {@code value}, {@code verbose}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#setProperty(String, Object, boolean)}
   */
  @Test
  public void testSetPropertyWithNameValueVerbose_givenPropertyHelperProjectIsProject() {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.setProject(new Project());
    propertyHelper.add(LocalProperties.get(new Project()));

    // Act
    propertyHelper.setProperty("Name", "Value", true);

    // Assert
    Hashtable<String, Object> internalProperties = propertyHelper.getInternalProperties();
    assertEquals(1, internalProperties.size());
    assertEquals("Value", internalProperties.get("Name"));
    Set<String> propertyNames = propertyHelper.getPropertyNames();
    assertEquals(1, propertyNames.size());
    assertTrue(propertyNames.contains("Name"));
    assertEquals(internalProperties, propertyHelper.getProperties());
  }

  /**
   * Test {@link PropertyHelper#setProperty(String, Object, boolean)} with {@code name}, {@code value}, {@code verbose}.
   * <ul>
   *   <li>Then {@link PropertyHelper#PropertyHelper()} PropertyNames Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#setProperty(String, Object, boolean)}
   */
  @Test
  public void testSetPropertyWithNameValueVerbose_thenPropertyHelperPropertyNamesEmpty() {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();

    // Act
    boolean actualSetPropertyResult = propertyHelper.setProperty("Name", null, true);

    // Assert
    assertTrue(propertyHelper.getPropertyNames().isEmpty());
    assertTrue(actualSetPropertyResult);
  }

  /**
   * Test {@link PropertyHelper#setProperty(String, String, Object, boolean)} with {@code ns}, {@code name}, {@code value}, {@code verbose}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#setProperty(String, String, Object, boolean)}
   */
  @Test
  public void testSetPropertyWithNsNameValueVerbose_givenJavaLangObject() {
    // Arrange
    Project p = new Project();
    Class<Object> typeClass = Object.class;
    p.addDataTypeDefinition(" -> ", typeClass);
    p.addBuildListener(new AntClassLoader());

    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.setProject(p);
    propertyHelper.add(LocalProperties.get(new Project()));

    // Act
    propertyHelper.setProperty("Ns", "Name", "Value", true);

    // Assert
    Hashtable<String, Object> internalProperties = propertyHelper.getInternalProperties();
    assertEquals(1, internalProperties.size());
    assertEquals("Value", internalProperties.get("Name"));
    Set<String> propertyNames = propertyHelper.getPropertyNames();
    assertEquals(1, propertyNames.size());
    assertTrue(propertyNames.contains("Name"));
    assertEquals(internalProperties, propertyHelper.getProperties());
  }

  /**
   * Test {@link PropertyHelper#setProperty(String, String, Object, boolean)} with {@code ns}, {@code name}, {@code value}, {@code verbose}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#setProperty(String, String, Object, boolean)}
   */
  @Test
  public void testSetPropertyWithNsNameValueVerbose_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project p = new Project();
    p.addBuildListener(new AntClassLoader());

    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.setProject(p);
    propertyHelper.add(LocalProperties.get(new Project()));

    // Act
    propertyHelper.setProperty("Ns", "Name", "Value", true);

    // Assert
    Hashtable<String, Object> internalProperties = propertyHelper.getInternalProperties();
    assertEquals(1, internalProperties.size());
    assertEquals("Value", internalProperties.get("Name"));
    Set<String> propertyNames = propertyHelper.getPropertyNames();
    assertEquals(1, propertyNames.size());
    assertTrue(propertyNames.contains("Name"));
    assertEquals(internalProperties, propertyHelper.getProperties());
  }

  /**
   * Test {@link PropertyHelper#setProperty(String, String, Object, boolean)} with {@code ns}, {@code name}, {@code value}, {@code verbose}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#setProperty(String, String, Object, boolean)}
   */
  @Test
  public void testSetPropertyWithNsNameValueVerbose_givenPropertyHelper() {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();

    // Act
    propertyHelper.setProperty("Ns", "Name", "Value", true);

    // Assert
    Hashtable<String, Object> internalProperties = propertyHelper.getInternalProperties();
    assertEquals(1, internalProperties.size());
    assertEquals("Value", internalProperties.get("Name"));
    Set<String> propertyNames = propertyHelper.getPropertyNames();
    assertEquals(1, propertyNames.size());
    assertTrue(propertyNames.contains("Name"));
    assertEquals(internalProperties, propertyHelper.getProperties());
  }

  /**
   * Test {@link PropertyHelper#setProperty(String, String, Object, boolean)} with {@code ns}, {@code name}, {@code value}, {@code verbose}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()} add {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#setProperty(String, String, Object, boolean)}
   */
  @Test
  public void testSetPropertyWithNsNameValueVerbose_givenPropertyHelperAddProject() {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.add(LocalProperties.get(new Project()));

    // Act
    propertyHelper.setProperty("Ns", "Name", "Value", true);

    // Assert
    Hashtable<String, Object> internalProperties = propertyHelper.getInternalProperties();
    assertEquals(1, internalProperties.size());
    assertEquals("Value", internalProperties.get("Name"));
    Set<String> propertyNames = propertyHelper.getPropertyNames();
    assertEquals(1, propertyNames.size());
    assertTrue(propertyNames.contains("Name"));
    assertEquals(internalProperties, propertyHelper.getProperties());
  }

  /**
   * Test {@link PropertyHelper#setProperty(String, String, Object, boolean)} with {@code ns}, {@code name}, {@code value}, {@code verbose}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#setProperty(String, String, Object, boolean)}
   */
  @Test
  public void testSetPropertyWithNsNameValueVerbose_givenPropertyHelperProjectIsProject() {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.setProject(new Project());
    propertyHelper.add(LocalProperties.get(new Project()));

    // Act
    propertyHelper.setProperty("Ns", "Name", "Value", true);

    // Assert
    Hashtable<String, Object> internalProperties = propertyHelper.getInternalProperties();
    assertEquals(1, internalProperties.size());
    assertEquals("Value", internalProperties.get("Name"));
    Set<String> propertyNames = propertyHelper.getPropertyNames();
    assertEquals(1, propertyNames.size());
    assertTrue(propertyNames.contains("Name"));
    assertEquals(internalProperties, propertyHelper.getProperties());
  }

  /**
   * Test {@link PropertyHelper#setProperty(String, String, Object, boolean)} with {@code ns}, {@code name}, {@code value}, {@code verbose}.
   * <ul>
   *   <li>Then {@link PropertyHelper#PropertyHelper()} PropertyNames Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#setProperty(String, String, Object, boolean)}
   */
  @Test
  public void testSetPropertyWithNsNameValueVerbose_thenPropertyHelperPropertyNamesEmpty() {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();

    // Act
    boolean actualSetPropertyResult = propertyHelper.setProperty("Ns", "Name", null, true);

    // Assert
    assertTrue(propertyHelper.getPropertyNames().isEmpty());
    assertTrue(actualSetPropertyResult);
  }

  /**
   * Test {@link PropertyHelper#setNewProperty(String, Object)} with {@code name}, {@code value}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#setNewProperty(String, Object)}
   */
  @Test
  public void testSetNewPropertyWithNameValue_givenJavaLangObject() {
    // Arrange
    Project p = new Project();
    Class<Object> typeClass = Object.class;
    p.addDataTypeDefinition(" -> ", typeClass);
    p.addBuildListener(new AntClassLoader());

    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.setProject(p);
    propertyHelper.add(LocalProperties.get(new Project()));

    // Act
    propertyHelper.setNewProperty("Name", "Value");

    // Assert
    Hashtable<String, Object> internalProperties = propertyHelper.getInternalProperties();
    assertEquals(1, internalProperties.size());
    assertEquals("Value", internalProperties.get("Name"));
    Set<String> propertyNames = propertyHelper.getPropertyNames();
    assertEquals(1, propertyNames.size());
    assertTrue(propertyNames.contains("Name"));
    assertEquals(internalProperties, propertyHelper.getProperties());
  }

  /**
   * Test {@link PropertyHelper#setNewProperty(String, Object)} with {@code name}, {@code value}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#setNewProperty(String, Object)}
   */
  @Test
  public void testSetNewPropertyWithNameValue_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project p = new Project();
    p.addBuildListener(new AntClassLoader());

    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.setProject(p);
    propertyHelper.add(LocalProperties.get(new Project()));

    // Act
    propertyHelper.setNewProperty("Name", "Value");

    // Assert
    Hashtable<String, Object> internalProperties = propertyHelper.getInternalProperties();
    assertEquals(1, internalProperties.size());
    assertEquals("Value", internalProperties.get("Name"));
    Set<String> propertyNames = propertyHelper.getPropertyNames();
    assertEquals(1, propertyNames.size());
    assertTrue(propertyNames.contains("Name"));
    assertEquals(internalProperties, propertyHelper.getProperties());
  }

  /**
   * Test {@link PropertyHelper#setNewProperty(String, Object)} with {@code name}, {@code value}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#setNewProperty(String, Object)}
   */
  @Test
  public void testSetNewPropertyWithNameValue_givenPropertyHelper() {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();

    // Act
    propertyHelper.setNewProperty("Name", "Value");

    // Assert
    Hashtable<String, Object> internalProperties = propertyHelper.getInternalProperties();
    assertEquals(1, internalProperties.size());
    assertEquals("Value", internalProperties.get("Name"));
    Set<String> propertyNames = propertyHelper.getPropertyNames();
    assertEquals(1, propertyNames.size());
    assertTrue(propertyNames.contains("Name"));
    assertEquals(internalProperties, propertyHelper.getProperties());
  }

  /**
   * Test {@link PropertyHelper#setNewProperty(String, Object)} with {@code name}, {@code value}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()} add {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#setNewProperty(String, Object)}
   */
  @Test
  public void testSetNewPropertyWithNameValue_givenPropertyHelperAddProject() {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.add(LocalProperties.get(new Project()));

    // Act
    propertyHelper.setNewProperty("Name", "Value");

    // Assert
    Hashtable<String, Object> internalProperties = propertyHelper.getInternalProperties();
    assertEquals(1, internalProperties.size());
    assertEquals("Value", internalProperties.get("Name"));
    Set<String> propertyNames = propertyHelper.getPropertyNames();
    assertEquals(1, propertyNames.size());
    assertTrue(propertyNames.contains("Name"));
    assertEquals(internalProperties, propertyHelper.getProperties());
  }

  /**
   * Test {@link PropertyHelper#setNewProperty(String, Object)} with {@code name}, {@code value}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#setNewProperty(String, Object)}
   */
  @Test
  public void testSetNewPropertyWithNameValue_givenPropertyHelperProjectIsProject() {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.setProject(new Project());
    propertyHelper.add(LocalProperties.get(new Project()));

    // Act
    propertyHelper.setNewProperty("Name", "Value");

    // Assert
    Hashtable<String, Object> internalProperties = propertyHelper.getInternalProperties();
    assertEquals(1, internalProperties.size());
    assertEquals("Value", internalProperties.get("Name"));
    Set<String> propertyNames = propertyHelper.getPropertyNames();
    assertEquals(1, propertyNames.size());
    assertTrue(propertyNames.contains("Name"));
    assertEquals(internalProperties, propertyHelper.getProperties());
  }

  /**
   * Test {@link PropertyHelper#setNewProperty(String, Object)} with {@code name}, {@code value}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link PropertyHelper#PropertyHelper()} PropertyNames Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#setNewProperty(String, Object)}
   */
  @Test
  public void testSetNewPropertyWithNameValue_whenNull_thenPropertyHelperPropertyNamesEmpty() {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();

    // Act
    propertyHelper.setNewProperty(null, "Value");

    // Assert that nothing has changed
    assertTrue(propertyHelper.getPropertyNames().isEmpty());
  }

  /**
   * Test {@link PropertyHelper#setNewProperty(String, Object)} with {@code name}, {@code value}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link PropertyHelper#PropertyHelper()} PropertyNames Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#setNewProperty(String, Object)}
   */
  @Test
  public void testSetNewPropertyWithNameValue_whenNull_thenPropertyHelperPropertyNamesEmpty2() {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();

    // Act
    propertyHelper.setNewProperty("Name", null);

    // Assert that nothing has changed
    assertTrue(propertyHelper.getPropertyNames().isEmpty());
  }

  /**
   * Test {@link PropertyHelper#setNewProperty(String, String, Object)} with {@code ns}, {@code name}, {@code value}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#setNewProperty(String, String, Object)}
   */
  @Test
  public void testSetNewPropertyWithNsNameValue_givenJavaLangObject() {
    // Arrange
    Project p = new Project();
    Class<Object> typeClass = Object.class;
    p.addDataTypeDefinition(" -> ", typeClass);
    p.addBuildListener(new AntClassLoader());

    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.setProject(p);
    propertyHelper.add(LocalProperties.get(new Project()));

    // Act
    propertyHelper.setNewProperty("Ns", "Name", "Value");

    // Assert
    Hashtable<String, Object> internalProperties = propertyHelper.getInternalProperties();
    assertEquals(1, internalProperties.size());
    assertEquals("Value", internalProperties.get("Name"));
    Set<String> propertyNames = propertyHelper.getPropertyNames();
    assertEquals(1, propertyNames.size());
    assertTrue(propertyNames.contains("Name"));
    assertEquals(internalProperties, propertyHelper.getProperties());
  }

  /**
   * Test {@link PropertyHelper#setNewProperty(String, String, Object)} with {@code ns}, {@code name}, {@code value}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#setNewProperty(String, String, Object)}
   */
  @Test
  public void testSetNewPropertyWithNsNameValue_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project p = new Project();
    p.addBuildListener(new AntClassLoader());

    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.setProject(p);
    propertyHelper.add(LocalProperties.get(new Project()));

    // Act
    propertyHelper.setNewProperty("Ns", "Name", "Value");

    // Assert
    Hashtable<String, Object> internalProperties = propertyHelper.getInternalProperties();
    assertEquals(1, internalProperties.size());
    assertEquals("Value", internalProperties.get("Name"));
    Set<String> propertyNames = propertyHelper.getPropertyNames();
    assertEquals(1, propertyNames.size());
    assertTrue(propertyNames.contains("Name"));
    assertEquals(internalProperties, propertyHelper.getProperties());
  }

  /**
   * Test {@link PropertyHelper#setNewProperty(String, String, Object)} with {@code ns}, {@code name}, {@code value}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#setNewProperty(String, String, Object)}
   */
  @Test
  public void testSetNewPropertyWithNsNameValue_givenPropertyHelper() {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();

    // Act
    propertyHelper.setNewProperty("Ns", "Name", "Value");

    // Assert
    Hashtable<String, Object> internalProperties = propertyHelper.getInternalProperties();
    assertEquals(1, internalProperties.size());
    assertEquals("Value", internalProperties.get("Name"));
    Set<String> propertyNames = propertyHelper.getPropertyNames();
    assertEquals(1, propertyNames.size());
    assertTrue(propertyNames.contains("Name"));
    assertEquals(internalProperties, propertyHelper.getProperties());
  }

  /**
   * Test {@link PropertyHelper#setNewProperty(String, String, Object)} with {@code ns}, {@code name}, {@code value}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()} add {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#setNewProperty(String, String, Object)}
   */
  @Test
  public void testSetNewPropertyWithNsNameValue_givenPropertyHelperAddProject() {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.add(LocalProperties.get(new Project()));

    // Act
    propertyHelper.setNewProperty("Ns", "Name", "Value");

    // Assert
    Hashtable<String, Object> internalProperties = propertyHelper.getInternalProperties();
    assertEquals(1, internalProperties.size());
    assertEquals("Value", internalProperties.get("Name"));
    Set<String> propertyNames = propertyHelper.getPropertyNames();
    assertEquals(1, propertyNames.size());
    assertTrue(propertyNames.contains("Name"));
    assertEquals(internalProperties, propertyHelper.getProperties());
  }

  /**
   * Test {@link PropertyHelper#setNewProperty(String, String, Object)} with {@code ns}, {@code name}, {@code value}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#setNewProperty(String, String, Object)}
   */
  @Test
  public void testSetNewPropertyWithNsNameValue_givenPropertyHelperProjectIsProject() {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.setProject(new Project());
    propertyHelper.add(LocalProperties.get(new Project()));

    // Act
    propertyHelper.setNewProperty("Ns", "Name", "Value");

    // Assert
    Hashtable<String, Object> internalProperties = propertyHelper.getInternalProperties();
    assertEquals(1, internalProperties.size());
    assertEquals("Value", internalProperties.get("Name"));
    Set<String> propertyNames = propertyHelper.getPropertyNames();
    assertEquals(1, propertyNames.size());
    assertTrue(propertyNames.contains("Name"));
    assertEquals(internalProperties, propertyHelper.getProperties());
  }

  /**
   * Test {@link PropertyHelper#setNewProperty(String, String, Object)} with {@code ns}, {@code name}, {@code value}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link PropertyHelper#PropertyHelper()} PropertyNames Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#setNewProperty(String, String, Object)}
   */
  @Test
  public void testSetNewPropertyWithNsNameValue_whenNull_thenPropertyHelperPropertyNamesEmpty() {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();

    // Act
    propertyHelper.setNewProperty("Ns", null, "Value");

    // Assert that nothing has changed
    assertTrue(propertyHelper.getPropertyNames().isEmpty());
  }

  /**
   * Test {@link PropertyHelper#setNewProperty(String, String, Object)} with {@code ns}, {@code name}, {@code value}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link PropertyHelper#PropertyHelper()} PropertyNames Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#setNewProperty(String, String, Object)}
   */
  @Test
  public void testSetNewPropertyWithNsNameValue_whenNull_thenPropertyHelperPropertyNamesEmpty2() {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();

    // Act
    propertyHelper.setNewProperty("Ns", "Name", null);

    // Assert that nothing has changed
    assertTrue(propertyHelper.getPropertyNames().isEmpty());
  }

  /**
   * Test {@link PropertyHelper#setUserProperty(String, Object)} with {@code name}, {@code value}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#setUserProperty(String, Object)}
   */
  @Test
  public void testSetUserPropertyWithNameValue_givenJavaLangObject() {
    // Arrange
    Project p = new Project();
    Class<Object> typeClass = Object.class;
    p.addDataTypeDefinition(" -> ", typeClass);
    p.addBuildListener(new AntClassLoader());

    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.setProject(p);

    // Act
    propertyHelper.setUserProperty("Name", "Value");

    // Assert
    Hashtable<String, Object> internalProperties = propertyHelper.getInternalProperties();
    assertEquals(1, internalProperties.size());
    assertEquals("Value", internalProperties.get("Name"));
    Set<String> propertyNames = propertyHelper.getPropertyNames();
    assertEquals(1, propertyNames.size());
    assertTrue(propertyNames.contains("Name"));
    assertEquals(internalProperties, propertyHelper.getInternalUserProperties());
    assertEquals(internalProperties, propertyHelper.getProperties());
    assertEquals(internalProperties, propertyHelper.getUserProperties());
  }

  /**
   * Test {@link PropertyHelper#setUserProperty(String, Object)} with {@code name}, {@code value}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#setUserProperty(String, Object)}
   */
  @Test
  public void testSetUserPropertyWithNameValue_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project p = new Project();
    p.addBuildListener(new AntClassLoader());

    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.setProject(p);

    // Act
    propertyHelper.setUserProperty("Name", "Value");

    // Assert
    Hashtable<String, Object> internalProperties = propertyHelper.getInternalProperties();
    assertEquals(1, internalProperties.size());
    assertEquals("Value", internalProperties.get("Name"));
    Set<String> propertyNames = propertyHelper.getPropertyNames();
    assertEquals(1, propertyNames.size());
    assertTrue(propertyNames.contains("Name"));
    assertEquals(internalProperties, propertyHelper.getInternalUserProperties());
    assertEquals(internalProperties, propertyHelper.getProperties());
    assertEquals(internalProperties, propertyHelper.getUserProperties());
  }

  /**
   * Test {@link PropertyHelper#setUserProperty(String, Object)} with {@code name}, {@code value}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link DefaultLogger} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#setUserProperty(String, Object)}
   */
  @Test
  public void testSetUserPropertyWithNameValue_givenProjectAddBuildListenerDefaultLogger() {
    // Arrange
    Project p = new Project();
    p.addBuildListener(new DefaultLogger());

    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.setProject(p);

    // Act
    propertyHelper.setUserProperty("Name", "Value");

    // Assert
    Hashtable<String, Object> internalProperties = propertyHelper.getInternalProperties();
    assertEquals(1, internalProperties.size());
    assertEquals("Value", internalProperties.get("Name"));
    Set<String> propertyNames = propertyHelper.getPropertyNames();
    assertEquals(1, propertyNames.size());
    assertTrue(propertyNames.contains("Name"));
    assertEquals(internalProperties, propertyHelper.getInternalUserProperties());
    assertEquals(internalProperties, propertyHelper.getProperties());
    assertEquals(internalProperties, propertyHelper.getUserProperties());
  }

  /**
   * Test {@link PropertyHelper#setUserProperty(String, Object)} with {@code name}, {@code value}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link ExecutorTest} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#setUserProperty(String, Object)}
   */
  @Test
  public void testSetUserPropertyWithNameValue_givenProjectAddBuildListenerExecutorTest() {
    // Arrange
    Project p = new Project();
    p.addBuildListener(new ExecutorTest());

    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.setProject(p);

    // Act
    propertyHelper.setUserProperty("Name", "Value");

    // Assert
    Hashtable<String, Object> internalProperties = propertyHelper.getInternalProperties();
    assertEquals(1, internalProperties.size());
    assertEquals("Value", internalProperties.get("Name"));
    Set<String> propertyNames = propertyHelper.getPropertyNames();
    assertEquals(1, propertyNames.size());
    assertTrue(propertyNames.contains("Name"));
    assertEquals(internalProperties, propertyHelper.getInternalUserProperties());
    assertEquals(internalProperties, propertyHelper.getProperties());
    assertEquals(internalProperties, propertyHelper.getUserProperties());
  }

  /**
   * Test {@link PropertyHelper#setUserProperty(String, Object)} with {@code name}, {@code value}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#setUserProperty(String, Object)}
   */
  @Test
  public void testSetUserPropertyWithNameValue_givenPropertyHelper() {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();

    // Act
    propertyHelper.setUserProperty("Name", "Value");

    // Assert
    Hashtable<String, Object> internalProperties = propertyHelper.getInternalProperties();
    assertEquals(1, internalProperties.size());
    assertEquals("Value", internalProperties.get("Name"));
    Set<String> propertyNames = propertyHelper.getPropertyNames();
    assertEquals(1, propertyNames.size());
    assertTrue(propertyNames.contains("Name"));
    assertEquals(internalProperties, propertyHelper.getInternalUserProperties());
    assertEquals(internalProperties, propertyHelper.getProperties());
    assertEquals(internalProperties, propertyHelper.getUserProperties());
  }

  /**
   * Test {@link PropertyHelper#setUserProperty(String, Object)} with {@code name}, {@code value}.
   * <ul>
   *   <li>Then {@link PropertyHelper#PropertyHelper()} InternalProperties size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#setUserProperty(String, Object)}
   */
  @Test
  public void testSetUserPropertyWithNameValue_thenPropertyHelperInternalPropertiesSizeIsOne() {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.setProject(new Project());

    // Act
    propertyHelper.setUserProperty("Name", "Value");

    // Assert
    Hashtable<String, Object> internalProperties = propertyHelper.getInternalProperties();
    assertEquals(1, internalProperties.size());
    assertEquals("Value", internalProperties.get("Name"));
    Set<String> propertyNames = propertyHelper.getPropertyNames();
    assertEquals(1, propertyNames.size());
    assertTrue(propertyNames.contains("Name"));
    assertEquals(internalProperties, propertyHelper.getInternalUserProperties());
    assertEquals(internalProperties, propertyHelper.getProperties());
    assertEquals(internalProperties, propertyHelper.getUserProperties());
  }

  /**
   * Test {@link PropertyHelper#setUserProperty(String, String, Object)} with {@code ns}, {@code name}, {@code value}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#setUserProperty(String, String, Object)}
   */
  @Test
  public void testSetUserPropertyWithNsNameValue_givenJavaLangObject() {
    // Arrange
    Project p = new Project();
    Class<Object> typeClass = Object.class;
    p.addDataTypeDefinition(" -> ", typeClass);
    p.addBuildListener(new AntClassLoader());

    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.setProject(p);

    // Act
    propertyHelper.setUserProperty("Ns", "Name", "Value");

    // Assert
    Hashtable<String, Object> internalProperties = propertyHelper.getInternalProperties();
    assertEquals(1, internalProperties.size());
    assertEquals("Value", internalProperties.get("Name"));
    Set<String> propertyNames = propertyHelper.getPropertyNames();
    assertEquals(1, propertyNames.size());
    assertTrue(propertyNames.contains("Name"));
    assertEquals(internalProperties, propertyHelper.getInternalUserProperties());
    assertEquals(internalProperties, propertyHelper.getProperties());
    assertEquals(internalProperties, propertyHelper.getUserProperties());
  }

  /**
   * Test {@link PropertyHelper#setUserProperty(String, String, Object)} with {@code ns}, {@code name}, {@code value}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#setUserProperty(String, String, Object)}
   */
  @Test
  public void testSetUserPropertyWithNsNameValue_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project p = new Project();
    p.addBuildListener(new AntClassLoader());

    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.setProject(p);

    // Act
    propertyHelper.setUserProperty("Ns", "Name", "Value");

    // Assert
    Hashtable<String, Object> internalProperties = propertyHelper.getInternalProperties();
    assertEquals(1, internalProperties.size());
    assertEquals("Value", internalProperties.get("Name"));
    Set<String> propertyNames = propertyHelper.getPropertyNames();
    assertEquals(1, propertyNames.size());
    assertTrue(propertyNames.contains("Name"));
    assertEquals(internalProperties, propertyHelper.getInternalUserProperties());
    assertEquals(internalProperties, propertyHelper.getProperties());
    assertEquals(internalProperties, propertyHelper.getUserProperties());
  }

  /**
   * Test {@link PropertyHelper#setUserProperty(String, String, Object)} with {@code ns}, {@code name}, {@code value}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link DefaultLogger} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#setUserProperty(String, String, Object)}
   */
  @Test
  public void testSetUserPropertyWithNsNameValue_givenProjectAddBuildListenerDefaultLogger() {
    // Arrange
    Project p = new Project();
    p.addBuildListener(new DefaultLogger());

    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.setProject(p);

    // Act
    propertyHelper.setUserProperty("Ns", "Name", "Value");

    // Assert
    Hashtable<String, Object> internalProperties = propertyHelper.getInternalProperties();
    assertEquals(1, internalProperties.size());
    assertEquals("Value", internalProperties.get("Name"));
    Set<String> propertyNames = propertyHelper.getPropertyNames();
    assertEquals(1, propertyNames.size());
    assertTrue(propertyNames.contains("Name"));
    assertEquals(internalProperties, propertyHelper.getInternalUserProperties());
    assertEquals(internalProperties, propertyHelper.getProperties());
    assertEquals(internalProperties, propertyHelper.getUserProperties());
  }

  /**
   * Test {@link PropertyHelper#setUserProperty(String, String, Object)} with {@code ns}, {@code name}, {@code value}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link ExecutorTest} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#setUserProperty(String, String, Object)}
   */
  @Test
  public void testSetUserPropertyWithNsNameValue_givenProjectAddBuildListenerExecutorTest() {
    // Arrange
    Project p = new Project();
    p.addBuildListener(new ExecutorTest());

    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.setProject(p);

    // Act
    propertyHelper.setUserProperty("Ns", "Name", "Value");

    // Assert
    Hashtable<String, Object> internalProperties = propertyHelper.getInternalProperties();
    assertEquals(1, internalProperties.size());
    assertEquals("Value", internalProperties.get("Name"));
    Set<String> propertyNames = propertyHelper.getPropertyNames();
    assertEquals(1, propertyNames.size());
    assertTrue(propertyNames.contains("Name"));
    assertEquals(internalProperties, propertyHelper.getInternalUserProperties());
    assertEquals(internalProperties, propertyHelper.getProperties());
    assertEquals(internalProperties, propertyHelper.getUserProperties());
  }

  /**
   * Test {@link PropertyHelper#setUserProperty(String, String, Object)} with {@code ns}, {@code name}, {@code value}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#setUserProperty(String, String, Object)}
   */
  @Test
  public void testSetUserPropertyWithNsNameValue_givenPropertyHelper() {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();

    // Act
    propertyHelper.setUserProperty("Ns", "Name", "Value");

    // Assert
    Hashtable<String, Object> internalProperties = propertyHelper.getInternalProperties();
    assertEquals(1, internalProperties.size());
    assertEquals("Value", internalProperties.get("Name"));
    Set<String> propertyNames = propertyHelper.getPropertyNames();
    assertEquals(1, propertyNames.size());
    assertTrue(propertyNames.contains("Name"));
    assertEquals(internalProperties, propertyHelper.getInternalUserProperties());
    assertEquals(internalProperties, propertyHelper.getProperties());
    assertEquals(internalProperties, propertyHelper.getUserProperties());
  }

  /**
   * Test {@link PropertyHelper#setUserProperty(String, String, Object)} with {@code ns}, {@code name}, {@code value}.
   * <ul>
   *   <li>Then {@link PropertyHelper#PropertyHelper()} InternalProperties size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#setUserProperty(String, String, Object)}
   */
  @Test
  public void testSetUserPropertyWithNsNameValue_thenPropertyHelperInternalPropertiesSizeIsOne() {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.setProject(new Project());

    // Act
    propertyHelper.setUserProperty("Ns", "Name", "Value");

    // Assert
    Hashtable<String, Object> internalProperties = propertyHelper.getInternalProperties();
    assertEquals(1, internalProperties.size());
    assertEquals("Value", internalProperties.get("Name"));
    Set<String> propertyNames = propertyHelper.getPropertyNames();
    assertEquals(1, propertyNames.size());
    assertTrue(propertyNames.contains("Name"));
    assertEquals(internalProperties, propertyHelper.getInternalUserProperties());
    assertEquals(internalProperties, propertyHelper.getProperties());
    assertEquals(internalProperties, propertyHelper.getUserProperties());
  }

  /**
   * Test {@link PropertyHelper#getPropertyNames()}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#getPropertyNames()}
   */
  @Test
  public void testGetPropertyNames_givenPropertyHelper() {
    // Arrange, Act and Assert
    assertTrue((new PropertyHelper()).getPropertyNames().isEmpty());
  }

  /**
   * Test {@link PropertyHelper#getPropertyNames()}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()} add {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#getPropertyNames()}
   */
  @Test
  public void testGetPropertyNames_givenPropertyHelperAddProject() {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.add(LocalProperties.get(new Project()));

    // Act and Assert
    assertTrue(propertyHelper.getPropertyNames().isEmpty());
  }

  /**
   * Test {@link PropertyHelper#getPropertyNames()}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()} add {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#getPropertyNames()}
   */
  @Test
  public void testGetPropertyNames_givenPropertyHelperAddProject2() {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.add(LocalProperties.get(new Project()));
    propertyHelper.add(LocalProperties.get(new Project()));

    // Act and Assert
    assertTrue(propertyHelper.getPropertyNames().isEmpty());
  }

  /**
   * Test {@link PropertyHelper#getUserProperty(String)} with {@code name}.
   * <ul>
   *   <li>When {@code Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#getUserProperty(String)}
   */
  @Test
  public void testGetUserPropertyWithName_whenName() {
    // Arrange, Act and Assert
    assertNull((new PropertyHelper()).getUserProperty("Name"));
  }

  /**
   * Test {@link PropertyHelper#getUserProperty(String)} with {@code name}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#getUserProperty(String)}
   */
  @Test
  public void testGetUserPropertyWithName_whenNull() {
    // Arrange, Act and Assert
    assertNull((new PropertyHelper()).getUserProperty(null));
  }

  /**
   * Test {@link PropertyHelper#getUserProperty(String, String)} with {@code ns}, {@code name}.
   * <ul>
   *   <li>When {@code Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#getUserProperty(String, String)}
   */
  @Test
  public void testGetUserPropertyWithNsName_whenName() {
    // Arrange, Act and Assert
    assertNull((new PropertyHelper()).getUserProperty("Ns", "Name"));
  }

  /**
   * Test {@link PropertyHelper#getUserProperty(String, String)} with {@code ns}, {@code name}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#getUserProperty(String, String)}
   */
  @Test
  public void testGetUserPropertyWithNsName_whenNull() {
    // Arrange, Act and Assert
    assertNull((new PropertyHelper()).getUserProperty("Ns", null));
  }

  /**
   * Test {@link PropertyHelper#getPropertyHelper(Project)}.
   * <ul>
   *   <li>Given {@link Target#Target()}.</li>
   *   <li>Then return Project Targets size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#getPropertyHelper(Project)}
   */
  @Test
  public void testGetPropertyHelper_givenTarget_thenReturnProjectTargetsSizeIsOne() throws BuildException {
    // Arrange
    Project project = new Project();
    Target target = new Target();
    project.addTarget("Adding reference: ", target);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    Project project2 = PropertyHelper.getPropertyHelper(project).getProject();
    Hashtable<String, Target> targets = project2.getTargets();
    assertEquals(1, targets.size());
    Map<String, Target> copyOfTargets = project2.getCopyOfTargets();
    assertEquals(1, copyOfTargets.size());
    assertEquals(1, project2.getBuildListeners().size());
    assertSame(target, targets.get("Adding reference: "));
    assertSame(target, copyOfTargets.get("Adding reference: "));
  }

  /**
   * Test {@link PropertyHelper#getPropertyHelper(Project)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#getPropertyHelper(Project)}
   */
  @Test
  public void testGetPropertyHelper_whenNull_thenReturnProjectIsNull() {
    // Arrange and Act
    PropertyHelper actualPropertyHelper = PropertyHelper.getPropertyHelper(null);

    // Assert
    assertNull(actualPropertyHelper.getProject());
    assertTrue(actualPropertyHelper.getPropertyNames().isEmpty());
  }

  /**
   * Test {@link PropertyHelper#getExpanders()}.
   * <p>
   * Method under test: {@link PropertyHelper#getExpanders()}
   */
  @Test
  public void testGetExpanders() {
    // Arrange and Act
    Collection<PropertyExpander> actualExpanders = (new PropertyHelper()).getExpanders();

    // Assert
    assertTrue(actualExpanders instanceof List);
    assertEquals(2, actualExpanders.size());
  }

  /**
   * Test {@link PropertyHelper#setPropertyHook(String, String, Object, boolean, boolean, boolean)}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#setPropertyHook(String, String, Object, boolean, boolean, boolean)}
   */
  @Test
  public void testSetPropertyHook_givenPropertyHelper() {
    // Arrange, Act and Assert
    assertFalse((new PropertyHelper()).setPropertyHook("Ns", "Name", "Value", true, true, true));
  }

  /**
   * Test {@link PropertyHelper#setPropertyHook(String, String, Object, boolean, boolean, boolean)}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()} Next is {@link PropertyHelper#PropertyHelper()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#setPropertyHook(String, String, Object, boolean, boolean, boolean)}
   */
  @Test
  public void testSetPropertyHook_givenPropertyHelperNextIsPropertyHelper() {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.setNext(new PropertyHelper());

    // Act and Assert
    assertFalse(propertyHelper.setPropertyHook("Ns", "Name", "Value", true, true, true));
  }

  /**
   * Test {@link PropertyHelper#getPropertyHook(String, String, boolean)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>When {@code toString:}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#getPropertyHook(String, String, boolean)}
   */
  @Test
  public void testGetPropertyHook_givenJavaLangObject_whenToString_thenReturnNull() {
    // Arrange
    Project p = new Project();
    Class<Object> typeClass = Object.class;
    p.addDataTypeDefinition(MagicNames.REFID_PROPERTY_HELPER, typeClass);
    p.addBuildListener(new AntClassLoader());

    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.setNext(null);
    propertyHelper.setProject(p);

    // Act and Assert
    assertNull(propertyHelper.getPropertyHook("Ns", "toString:", true));
  }

  /**
   * Test {@link PropertyHelper#getPropertyHook(String, String, boolean)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>When {@code toString:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#getPropertyHook(String, String, boolean)}
   */
  @Test
  public void testGetPropertyHook_givenProjectAddBuildListenerAntClassLoader_whenToString() {
    // Arrange
    Project p = new Project();
    p.addBuildListener(new AntClassLoader());

    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.setNext(null);
    propertyHelper.setProject(p);

    // Act and Assert
    assertNull(propertyHelper.getPropertyHook("Ns", "toString:", true));
  }

  /**
   * Test {@link PropertyHelper#getPropertyHook(String, String, boolean)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addReference empty string and {@code Value}.</li>
   *   <li>Then return {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#getPropertyHook(String, String, boolean)}
   */
  @Test
  public void testGetPropertyHook_givenProjectAddReferenceEmptyStringAndValue_thenReturnValue() {
    // Arrange
    Project p = new Project();
    p.addReference("", "Value");
    p.addBuildListener(new AntClassLoader());

    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.setNext(null);
    propertyHelper.setProject(p);

    // Act and Assert
    assertEquals("Value", propertyHelper.getPropertyHook("Ns", "toString:", true));
  }

  /**
   * Test {@link PropertyHelper#getPropertyHook(String, String, boolean)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addReference {@link MagicNames#REFID_PROPERTY_HELPER} and {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#getPropertyHook(String, String, boolean)}
   */
  @Test
  public void testGetPropertyHook_givenProjectAddReferenceRefid_property_helperAndValue() {
    // Arrange
    Project p = new Project();
    p.addReference(MagicNames.REFID_PROPERTY_HELPER, "Value");
    p.addBuildListener(new AntClassLoader());

    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.setNext(null);
    propertyHelper.setProject(p);

    // Act and Assert
    assertNull(propertyHelper.getPropertyHook("Ns", "toString:", true));
  }

  /**
   * Test {@link PropertyHelper#getPropertyHook(String, String, boolean)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@link MagicNames#REFID_PROPERTY_HELPER} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#getPropertyHook(String, String, boolean)}
   */
  @Test
  public void testGetPropertyHook_givenProjectAddTargetRefid_property_helperAndTarget() throws BuildException {
    // Arrange
    Project p = new Project();
    p.addTarget(MagicNames.REFID_PROPERTY_HELPER, new Target());
    p.addBuildListener(new AntClassLoader());

    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.setNext(null);
    propertyHelper.setProject(p);

    // Act and Assert
    assertNull(propertyHelper.getPropertyHook("Ns", "toString:", true));
  }

  /**
   * Test {@link PropertyHelper#getPropertyHook(String, String, boolean)}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()} Next is {@code null}.</li>
   *   <li>When {@code toString:}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#getPropertyHook(String, String, boolean)}
   */
  @Test
  public void testGetPropertyHook_givenPropertyHelperNextIsNull_whenToString_thenReturnNull() {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.setNext(null);
    propertyHelper.setProject(new Project());

    // Act and Assert
    assertNull(propertyHelper.getPropertyHook("Ns", "toString:", true));
  }

  /**
   * Test {@link PropertyHelper#getPropertyHook(String, String, boolean)}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()} Next is {@link PropertyHelper#PropertyHelper()}.</li>
   *   <li>When {@code toString:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#getPropertyHook(String, String, boolean)}
   */
  @Test
  public void testGetPropertyHook_givenPropertyHelperNextIsPropertyHelper_whenToString() {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.setNext(new PropertyHelper());
    propertyHelper.setProject(null);

    // Act and Assert
    assertNull(propertyHelper.getPropertyHook("Ns", "toString:", true));
  }

  /**
   * Test {@link PropertyHelper#getPropertyHook(String, String, boolean)}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()} Project is {@link Project} (default constructor).</li>
   *   <li>When {@code Name}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#getPropertyHook(String, String, boolean)}
   */
  @Test
  public void testGetPropertyHook_givenPropertyHelperProjectIsProject_whenName_thenReturnNull() {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.setProject(new Project());

    // Act and Assert
    assertNull(propertyHelper.getPropertyHook("Ns", "Name", true));
  }

  /**
   * Test {@link PropertyHelper#getPropertyHook(String, String, boolean)}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()}.</li>
   *   <li>When {@code Name}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#getPropertyHook(String, String, boolean)}
   */
  @Test
  public void testGetPropertyHook_givenPropertyHelper_whenName_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new PropertyHelper()).getPropertyHook("Ns", "Name", true));
  }

  /**
   * Test {@link PropertyHelper#parsePropertyString(String, Vector, Vector)}.
   * <ul>
   *   <li>Given empty string.</li>
   *   <li>Then {@link Stack} (default constructor) size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#parsePropertyString(String, Vector, Vector)}
   */
  @Test
  public void testParsePropertyString_givenEmptyString_thenStackSizeIsTwo() throws BuildException {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();

    Stack<String> fragments = new Stack<>();
    fragments.add("");
    Vector<String> propertyRefs = Execute.getProcEnvironment();

    // Act
    propertyHelper.parsePropertyString("42", fragments, propertyRefs);

    // Assert
    assertEquals(2, fragments.size());
    assertEquals("", fragments.get(0));
    assertEquals("42", fragments.get(1));
  }

  /**
   * Test {@link PropertyHelper#parsePropertyString(String, Vector, Vector)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then {@link Stack} (default constructor) Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#parsePropertyString(String, Vector, Vector)}
   */
  @Test
  public void testParsePropertyString_whenEmptyString_thenStackEmpty() throws BuildException {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();
    Stack<String> fragments = new Stack<>();
    Vector<String> propertyRefs = Execute.getProcEnvironment();

    // Act
    propertyHelper.parsePropertyString("", fragments, propertyRefs);

    // Assert that nothing has changed
    assertTrue(fragments.isEmpty());
  }

  /**
   * Test {@link PropertyHelper#parsePropertyString(String, Vector, Vector)}.
   * <ul>
   *   <li>When {@link Stack} (default constructor).</li>
   *   <li>Then {@link Stack} (default constructor) size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#parsePropertyString(String, Vector, Vector)}
   */
  @Test
  public void testParsePropertyString_whenStack_thenStackSizeIsOne() throws BuildException {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();
    Stack<String> fragments = new Stack<>();
    Vector<String> propertyRefs = Execute.getProcEnvironment();

    // Act
    propertyHelper.parsePropertyString("42", fragments, propertyRefs);

    // Assert
    assertEquals(1, fragments.size());
    assertEquals("42", fragments.get(0));
  }

  /**
   * Test {@link PropertyHelper#replaceProperties(String, String, Hashtable)} with {@code ns}, {@code value}, {@code keys}.
   * <p>
   * Method under test: {@link PropertyHelper#replaceProperties(String, String, Hashtable)}
   */
  @Test
  public void testReplacePropertiesWithNsValueKeys() throws BuildException {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();

    // Act and Assert
    assertEquals("org.apache.tools.ant.PropertyHelper$Delegate",
        propertyHelper.replaceProperties("Ns", "org.apache.tools.ant.PropertyHelper$Delegate", new Hashtable<>()));
  }

  /**
   * Test {@link PropertyHelper#replaceProperties(String, String, Hashtable)} with {@code ns}, {@code value}, {@code keys}.
   * <ul>
   *   <li>When {@code 42}.</li>
   *   <li>Then return {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#replaceProperties(String, String, Hashtable)}
   */
  @Test
  public void testReplacePropertiesWithNsValueKeys_when42_thenReturn42() throws BuildException {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();

    // Act and Assert
    assertEquals("42", propertyHelper.replaceProperties("Ns", "42", new Hashtable<>()));
  }

  /**
   * Test {@link PropertyHelper#replaceProperties(String, String, Hashtable)} with {@code ns}, {@code value}, {@code keys}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#replaceProperties(String, String, Hashtable)}
   */
  @Test
  public void testReplacePropertiesWithNsValueKeys_whenEmptyString_thenReturnEmptyString() throws BuildException {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();

    // Act and Assert
    assertEquals("", propertyHelper.replaceProperties("Ns", "", new Hashtable<>()));
  }

  /**
   * Test {@link PropertyHelper#replaceProperties(String, String, Hashtable)} with {@code ns}, {@code value}, {@code keys}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#replaceProperties(String, String, Hashtable)}
   */
  @Test
  public void testReplacePropertiesWithNsValueKeys_whenNull_thenReturnNull() throws BuildException {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();

    // Act and Assert
    assertNull(propertyHelper.replaceProperties("Ns", null, new Hashtable<>()));
  }

  /**
   * Test {@link PropertyHelper#replaceProperties(String, String, Hashtable)} with {@code ns}, {@code value}, {@code keys}.
   * <ul>
   *   <li>When {@code Value}.</li>
   *   <li>Then return {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#replaceProperties(String, String, Hashtable)}
   */
  @Test
  public void testReplacePropertiesWithNsValueKeys_whenValue_thenReturnValue() throws BuildException {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();

    // Act and Assert
    assertEquals("Value", propertyHelper.replaceProperties("Ns", "Value", new Hashtable<>()));
  }

  /**
   * Test {@link PropertyHelper#replaceProperties(String)} with {@code value}.
   * <ul>
   *   <li>Then return {@code PropertyHelper$Delegate}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#replaceProperties(String)}
   */
  @Test
  public void testReplacePropertiesWithValue_thenReturnOrgApacheToolsAntPropertyHelperDelegate() throws BuildException {
    // Arrange, Act and Assert
    assertEquals("org.apache.tools.ant.PropertyHelper$Delegate",
        (new PropertyHelper()).replaceProperties("org.apache.tools.ant.PropertyHelper$Delegate"));
  }

  /**
   * Test {@link PropertyHelper#replaceProperties(String)} with {@code value}.
   * <ul>
   *   <li>When {@code 42}.</li>
   *   <li>Then return {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#replaceProperties(String)}
   */
  @Test
  public void testReplacePropertiesWithValue_when42_thenReturn42() throws BuildException {
    // Arrange, Act and Assert
    assertEquals("42", (new PropertyHelper()).replaceProperties("42"));
  }

  /**
   * Test {@link PropertyHelper#replaceProperties(String)} with {@code value}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#replaceProperties(String)}
   */
  @Test
  public void testReplacePropertiesWithValue_whenEmptyString_thenReturnEmptyString() throws BuildException {
    // Arrange, Act and Assert
    assertEquals("", (new PropertyHelper()).replaceProperties(""));
  }

  /**
   * Test {@link PropertyHelper#replaceProperties(String)} with {@code value}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#replaceProperties(String)}
   */
  @Test
  public void testReplacePropertiesWithValue_whenNull_thenReturnNull() throws BuildException {
    // Arrange, Act and Assert
    assertNull((new PropertyHelper()).replaceProperties(null));
  }

  /**
   * Test {@link PropertyHelper#replaceProperties(String)} with {@code value}.
   * <ul>
   *   <li>When {@code Value}.</li>
   *   <li>Then return {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#replaceProperties(String)}
   */
  @Test
  public void testReplacePropertiesWithValue_whenValue_thenReturnValue() throws BuildException {
    // Arrange, Act and Assert
    assertEquals("Value", (new PropertyHelper()).replaceProperties("Value"));
  }

  /**
   * Test {@link PropertyHelper#parseProperties(String)}.
   * <ul>
   *   <li>Then return {@code PropertyHelper$Delegate}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#parseProperties(String)}
   */
  @Test
  public void testParseProperties_thenReturnOrgApacheToolsAntPropertyHelperDelegate() throws BuildException {
    // Arrange, Act and Assert
    assertEquals("org.apache.tools.ant.PropertyHelper$Delegate",
        (new PropertyHelper()).parseProperties("org.apache.tools.ant.PropertyHelper$Delegate"));
  }

  /**
   * Test {@link PropertyHelper#parseProperties(String)}.
   * <ul>
   *   <li>When {@code 42}.</li>
   *   <li>Then return {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#parseProperties(String)}
   */
  @Test
  public void testParseProperties_when42_thenReturn42() throws BuildException {
    // Arrange, Act and Assert
    assertEquals("42", (new PropertyHelper()).parseProperties("42"));
  }

  /**
   * Test {@link PropertyHelper#parseProperties(String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#parseProperties(String)}
   */
  @Test
  public void testParseProperties_whenEmptyString_thenReturnEmptyString() throws BuildException {
    // Arrange, Act and Assert
    assertEquals("", (new PropertyHelper()).parseProperties(""));
  }

  /**
   * Test {@link PropertyHelper#parseProperties(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#parseProperties(String)}
   */
  @Test
  public void testParseProperties_whenNull_thenReturnNull() throws BuildException {
    // Arrange, Act and Assert
    assertNull((new PropertyHelper()).parseProperties(null));
  }

  /**
   * Test {@link PropertyHelper#parseProperties(String)}.
   * <ul>
   *   <li>When {@code Value}.</li>
   *   <li>Then return {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#parseProperties(String)}
   */
  @Test
  public void testParseProperties_whenValue_thenReturnValue() throws BuildException {
    // Arrange, Act and Assert
    assertEquals("Value", (new PropertyHelper()).parseProperties("Value"));
  }

  /**
   * Test {@link PropertyHelper#containsProperties(String)}.
   * <ul>
   *   <li>When {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#containsProperties(String)}
   */
  @Test
  public void testContainsProperties_when42() {
    // Arrange, Act and Assert
    assertFalse((new PropertyHelper()).containsProperties("42"));
  }

  /**
   * Test {@link PropertyHelper#containsProperties(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#containsProperties(String)}
   */
  @Test
  public void testContainsProperties_whenNull() {
    // Arrange, Act and Assert
    assertFalse((new PropertyHelper()).containsProperties(null));
  }

  /**
   * Test {@link PropertyHelper#containsProperties(String)}.
   * <ul>
   *   <li>When {@code PropertyHelper$Delegate}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#containsProperties(String)}
   */
  @Test
  public void testContainsProperties_whenOrgApacheToolsAntPropertyHelperDelegate() {
    // Arrange, Act and Assert
    assertFalse((new PropertyHelper()).containsProperties("org.apache.tools.ant.PropertyHelper$Delegate"));
  }

  /**
   * Test {@link PropertyHelper#containsProperties(String)}.
   * <ul>
   *   <li>When {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#containsProperties(String)}
   */
  @Test
  public void testContainsProperties_whenValue() {
    // Arrange, Act and Assert
    assertFalse((new PropertyHelper()).containsProperties("Value"));
  }

  /**
   * Test {@link PropertyHelper#setInheritedProperty(String, Object)} with {@code name}, {@code value}.
   * <p>
   * Method under test: {@link PropertyHelper#setInheritedProperty(String, Object)}
   */
  @Test
  public void testSetInheritedPropertyWithNameValue() {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();

    // Act
    propertyHelper.setInheritedProperty("Name", "Value");

    // Assert
    Hashtable<String, Object> inheritedProperties = propertyHelper.getInheritedProperties();
    assertEquals(1, inheritedProperties.size());
    assertEquals("Value", inheritedProperties.get("Name"));
    Set<String> propertyNames = propertyHelper.getPropertyNames();
    assertEquals(1, propertyNames.size());
    assertTrue(propertyNames.contains("Name"));
  }

  /**
   * Test {@link PropertyHelper#setInheritedProperty(String, String, Object)} with {@code ns}, {@code name}, {@code value}.
   * <p>
   * Method under test: {@link PropertyHelper#setInheritedProperty(String, String, Object)}
   */
  @Test
  public void testSetInheritedPropertyWithNsNameValue() {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();

    // Act
    propertyHelper.setInheritedProperty("Ns", "Name", "Value");

    // Assert
    Hashtable<String, Object> inheritedProperties = propertyHelper.getInheritedProperties();
    assertEquals(1, inheritedProperties.size());
    assertEquals("Value", inheritedProperties.get("Name"));
    Set<String> propertyNames = propertyHelper.getPropertyNames();
    assertEquals(1, propertyNames.size());
    assertTrue(propertyNames.contains("Name"));
  }

  /**
   * Test {@link PropertyHelper#getProperties()}.
   * <p>
   * Method under test: {@link PropertyHelper#getProperties()}
   */
  @Test
  public void testGetProperties() {
    // Arrange, Act and Assert
    assertTrue((new PropertyHelper()).getProperties().isEmpty());
  }

  /**
   * Test {@link PropertyHelper#getUserProperties()}.
   * <p>
   * Method under test: {@link PropertyHelper#getUserProperties()}
   */
  @Test
  public void testGetUserProperties() {
    // Arrange, Act and Assert
    assertTrue((new PropertyHelper()).getUserProperties().isEmpty());
  }

  /**
   * Test {@link PropertyHelper#getInheritedProperties()}.
   * <p>
   * Method under test: {@link PropertyHelper#getInheritedProperties()}
   */
  @Test
  public void testGetInheritedProperties() {
    // Arrange, Act and Assert
    assertTrue((new PropertyHelper()).getInheritedProperties().isEmpty());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link PropertyHelper#setNext(PropertyHelper)}
   *   <li>{@link PropertyHelper#setProject(Project)}
   *   <li>{@link PropertyHelper#getInternalInheritedProperties()}
   *   <li>{@link PropertyHelper#getInternalProperties()}
   *   <li>{@link PropertyHelper#getInternalUserProperties()}
   *   <li>{@link PropertyHelper#getNext()}
   *   <li>{@link PropertyHelper#getProject()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();
    PropertyHelper next = new PropertyHelper();

    // Act
    propertyHelper.setNext(next);
    Project p = new Project();
    propertyHelper.setProject(p);
    Hashtable<String, Object> actualInternalInheritedProperties = propertyHelper.getInternalInheritedProperties();
    Hashtable<String, Object> actualInternalProperties = propertyHelper.getInternalProperties();
    Hashtable<String, Object> actualInternalUserProperties = propertyHelper.getInternalUserProperties();
    PropertyHelper actualNext = propertyHelper.getNext();
    Project actualProject = propertyHelper.getProject();

    // Assert
    assertTrue(actualInternalInheritedProperties.isEmpty());
    assertTrue(actualInternalProperties.isEmpty());
    assertTrue(actualInternalUserProperties.isEmpty());
    assertSame(p, actualProject);
    assertSame(next, actualNext);
  }

  /**
   * Test {@link PropertyHelper#parsePropertyStringDefault(String, Vector, Vector)}.
   * <ul>
   *   <li>Given empty string.</li>
   *   <li>Then {@link Stack} (default constructor) size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#parsePropertyStringDefault(String, Vector, Vector)}
   */
  @Test
  public void testParsePropertyStringDefault_givenEmptyString_thenStackSizeIsTwo() throws BuildException {
    // Arrange
    Stack<String> fragments = new Stack<>();
    fragments.add("");
    Vector<String> propertyRefs = Execute.getProcEnvironment();

    // Act
    PropertyHelper.parsePropertyStringDefault("42", fragments, propertyRefs);

    // Assert
    assertEquals(2, fragments.size());
    assertEquals("", fragments.get(0));
    assertEquals("42", fragments.get(1));
  }

  /**
   * Test {@link PropertyHelper#parsePropertyStringDefault(String, Vector, Vector)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then {@link Stack} (default constructor) Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#parsePropertyStringDefault(String, Vector, Vector)}
   */
  @Test
  public void testParsePropertyStringDefault_whenEmptyString_thenStackEmpty() throws BuildException {
    // Arrange
    Stack<String> fragments = new Stack<>();
    Vector<String> propertyRefs = Execute.getProcEnvironment();

    // Act
    PropertyHelper.parsePropertyStringDefault("", fragments, propertyRefs);

    // Assert that nothing has changed
    assertTrue(fragments.isEmpty());
  }

  /**
   * Test {@link PropertyHelper#parsePropertyStringDefault(String, Vector, Vector)}.
   * <ul>
   *   <li>When {@link Stack} (default constructor).</li>
   *   <li>Then {@link Stack} (default constructor) size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#parsePropertyStringDefault(String, Vector, Vector)}
   */
  @Test
  public void testParsePropertyStringDefault_whenStack_thenStackSizeIsOne() throws BuildException {
    // Arrange
    Stack<String> fragments = new Stack<>();
    Vector<String> propertyRefs = Execute.getProcEnvironment();

    // Act
    PropertyHelper.parsePropertyStringDefault("42", fragments, propertyRefs);

    // Assert
    assertEquals(1, fragments.size());
    assertEquals("42", fragments.get(0));
  }

  /**
   * Test {@link PropertyHelper#getDelegates(Class)}.
   * <ul>
   *   <li>Then return size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#getDelegates(Class)}
   */
  @Test
  public void testGetDelegates_thenReturnSizeIsTwo() {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();
    Class<PropertyEvaluator> type = PropertyEvaluator.class;

    // Act and Assert
    assertEquals(2, propertyHelper.getDelegates(type).size());
  }

  /**
   * Test {@link PropertyHelper#getDelegates(Class)}.
   * <ul>
   *   <li>When {@code PropertyHelper$Delegate}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#getDelegates(Class)}
   */
  @Test
  public void testGetDelegates_whenOrgApacheToolsAntPropertyHelperDelegate_thenReturnEmpty() {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();
    Class<Delegate> type = Delegate.class;

    // Act and Assert
    assertTrue(propertyHelper.getDelegates(type).isEmpty());
  }

  /**
   * Test {@link PropertyHelper#getDelegateInterfaces(Delegate)}.
   * <ul>
   *   <li>When {@link Project} (default constructor).</li>
   *   <li>Then return size is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#getDelegateInterfaces(Delegate)}
   */
  @Test
  public void testGetDelegateInterfaces_whenProject_thenReturnSizeIsThree() {
    // Arrange and Act
    Set<Class<? extends Delegate>> actualDelegateInterfaces = PropertyHelper
        .getDelegateInterfaces(LocalProperties.get(new Project()));

    // Assert
    assertEquals(3, actualDelegateInterfaces.size());
  }

  /**
   * Test {@link PropertyHelper#toBoolean(Object)}.
   * <ul>
   *   <li>When {@link Boolean#FALSE} toString.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#toBoolean(Object)}
   */
  @Test
  public void testToBoolean_whenFalseToString_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(PropertyHelper.toBoolean(Boolean.FALSE.toString()));
  }

  /**
   * Test {@link PropertyHelper#toBoolean(Object)}.
   * <ul>
   *   <li>When {@code false}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#toBoolean(Object)}
   */
  @Test
  public void testToBoolean_whenFalse_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(PropertyHelper.toBoolean(false));
  }

  /**
   * Test {@link PropertyHelper#toBoolean(Object)}.
   * <ul>
   *   <li>When forty-two.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#toBoolean(Object)}
   */
  @Test
  public void testToBoolean_whenFortyTwo_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull(PropertyHelper.toBoolean(42));
  }

  /**
   * Test {@link PropertyHelper#toBoolean(Object)}.
   * <ul>
   *   <li>When {@code no}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#toBoolean(Object)}
   */
  @Test
  public void testToBoolean_whenNo_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(PropertyHelper.toBoolean("no"));
  }

  /**
   * Test {@link PropertyHelper#toBoolean(Object)}.
   * <ul>
   *   <li>When {@code off}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#toBoolean(Object)}
   */
  @Test
  public void testToBoolean_whenOff_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(PropertyHelper.toBoolean("off"));
  }

  /**
   * Test {@link PropertyHelper#toBoolean(Object)}.
   * <ul>
   *   <li>When {@code on}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#toBoolean(Object)}
   */
  @Test
  public void testToBoolean_whenOn_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(PropertyHelper.toBoolean("on"));
  }

  /**
   * Test {@link PropertyHelper#toBoolean(Object)}.
   * <ul>
   *   <li>When {@link Boolean#TRUE} toString.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#toBoolean(Object)}
   */
  @Test
  public void testToBoolean_whenTrueToString_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(PropertyHelper.toBoolean(Boolean.TRUE.toString()));
  }

  /**
   * Test {@link PropertyHelper#toBoolean(Object)}.
   * <ul>
   *   <li>When {@code true}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#toBoolean(Object)}
   */
  @Test
  public void testToBoolean_whenTrue_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(PropertyHelper.toBoolean(true));
  }

  /**
   * Test {@link PropertyHelper#toBoolean(Object)}.
   * <ul>
   *   <li>When {@code Value}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#toBoolean(Object)}
   */
  @Test
  public void testToBoolean_whenValue_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull(PropertyHelper.toBoolean("Value"));
  }

  /**
   * Test {@link PropertyHelper#toBoolean(Object)}.
   * <ul>
   *   <li>When {@code yes}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#toBoolean(Object)}
   */
  @Test
  public void testToBoolean_whenYes_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(PropertyHelper.toBoolean("yes"));
  }

  /**
   * Test {@link PropertyHelper#testIfCondition(Object)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>When {@code toString:}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#testIfCondition(Object)}
   */
  @Test
  public void testTestIfCondition_givenJavaLangObject_whenToString_thenReturnFalse() {
    // Arrange
    Project p = new Project();
    Class<Object> typeClass = Object.class;
    p.addDataTypeDefinition("on", typeClass);
    p.addBuildListener(new AntClassLoader());

    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.setProject(p);

    // Act and Assert
    assertFalse(propertyHelper.testIfCondition("toString:"));
  }

  /**
   * Test {@link PropertyHelper#testIfCondition(Object)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>When {@code toString:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#testIfCondition(Object)}
   */
  @Test
  public void testTestIfCondition_givenProjectAddBuildListenerAntClassLoader_whenToString() {
    // Arrange
    Project p = new Project();
    p.addBuildListener(new AntClassLoader());

    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.setProject(p);

    // Act and Assert
    assertFalse(propertyHelper.testIfCondition("toString:"));
  }

  /**
   * Test {@link PropertyHelper#testIfCondition(Object)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addReference empty string and {@code Value}.</li>
   *   <li>When {@code toString:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#testIfCondition(Object)}
   */
  @Test
  public void testTestIfCondition_givenProjectAddReferenceEmptyStringAndValue_whenToString() {
    // Arrange
    Project p = new Project();
    p.addReference("", "Value");
    p.addBuildListener(new AntClassLoader());

    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.setProject(p);

    // Act and Assert
    assertTrue(propertyHelper.testIfCondition("toString:"));
  }

  /**
   * Test {@link PropertyHelper#testIfCondition(Object)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code on} and {@link Target#Target()}.</li>
   *   <li>When {@code toString:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#testIfCondition(Object)}
   */
  @Test
  public void testTestIfCondition_givenProjectAddTargetOnAndTarget_whenToString() throws BuildException {
    // Arrange
    Project p = new Project();
    p.addTarget("on", new Target());
    p.addBuildListener(new AntClassLoader());

    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.setProject(p);

    // Act and Assert
    assertFalse(propertyHelper.testIfCondition("toString:"));
  }

  /**
   * Test {@link PropertyHelper#testIfCondition(Object)}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()} add {@link Project} (default constructor).</li>
   *   <li>When {@code Value}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#testIfCondition(Object)}
   */
  @Test
  public void testTestIfCondition_givenPropertyHelperAddProject_whenValue_thenReturnFalse() {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.add(LocalProperties.get(new Project()));

    // Act and Assert
    assertFalse(propertyHelper.testIfCondition("Value"));
  }

  /**
   * Test {@link PropertyHelper#testIfCondition(Object)}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()} Project is {@link Project} (default constructor).</li>
   *   <li>When {@code ant.refid:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#testIfCondition(Object)}
   */
  @Test
  public void testTestIfCondition_givenPropertyHelperProjectIsProject_whenAntRefid() {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.setProject(new Project());

    // Act and Assert
    assertFalse(propertyHelper.testIfCondition("ant.refid:"));
  }

  /**
   * Test {@link PropertyHelper#testIfCondition(Object)}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()} Project is {@link Project} (default constructor).</li>
   *   <li>When {@code toString:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#testIfCondition(Object)}
   */
  @Test
  public void testTestIfCondition_givenPropertyHelperProjectIsProject_whenToString() {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.setProject(new Project());

    // Act and Assert
    assertFalse(propertyHelper.testIfCondition("toString:"));
  }

  /**
   * Test {@link PropertyHelper#testIfCondition(Object)}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()}.</li>
   *   <li>When {@code ant.refid:}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#testIfCondition(Object)}
   */
  @Test
  public void testTestIfCondition_givenPropertyHelper_whenAntRefid_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new PropertyHelper()).testIfCondition("ant.refid:"));
  }

  /**
   * Test {@link PropertyHelper#testIfCondition(Object)}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()}.</li>
   *   <li>When empty string.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#testIfCondition(Object)}
   */
  @Test
  public void testTestIfCondition_givenPropertyHelper_whenEmptyString_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new PropertyHelper()).testIfCondition(""));
  }

  /**
   * Test {@link PropertyHelper#testIfCondition(Object)}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()}.</li>
   *   <li>When {@link Boolean#FALSE} toString.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#testIfCondition(Object)}
   */
  @Test
  public void testTestIfCondition_givenPropertyHelper_whenFalseToString_thenReturnFalse() {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();

    // Act and Assert
    assertFalse(propertyHelper.testIfCondition(Boolean.FALSE.toString()));
  }

  /**
   * Test {@link PropertyHelper#testIfCondition(Object)}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()}.</li>
   *   <li>When forty-two.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#testIfCondition(Object)}
   */
  @Test
  public void testTestIfCondition_givenPropertyHelper_whenFortyTwo_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new PropertyHelper()).testIfCondition(42));
  }

  /**
   * Test {@link PropertyHelper#testIfCondition(Object)}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()}.</li>
   *   <li>When {@code no}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#testIfCondition(Object)}
   */
  @Test
  public void testTestIfCondition_givenPropertyHelper_whenNo_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new PropertyHelper()).testIfCondition("no"));
  }

  /**
   * Test {@link PropertyHelper#testIfCondition(Object)}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()}.</li>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#testIfCondition(Object)}
   */
  @Test
  public void testTestIfCondition_givenPropertyHelper_whenNull_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new PropertyHelper()).testIfCondition(null));
  }

  /**
   * Test {@link PropertyHelper#testIfCondition(Object)}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()}.</li>
   *   <li>When {@code off}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#testIfCondition(Object)}
   */
  @Test
  public void testTestIfCondition_givenPropertyHelper_whenOff_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new PropertyHelper()).testIfCondition("off"));
  }

  /**
   * Test {@link PropertyHelper#testIfCondition(Object)}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()}.</li>
   *   <li>When {@code on}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#testIfCondition(Object)}
   */
  @Test
  public void testTestIfCondition_givenPropertyHelper_whenOn_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new PropertyHelper()).testIfCondition("on"));
  }

  /**
   * Test {@link PropertyHelper#testIfCondition(Object)}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()}.</li>
   *   <li>When {@code toString:}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#testIfCondition(Object)}
   */
  @Test
  public void testTestIfCondition_givenPropertyHelper_whenToString_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new PropertyHelper()).testIfCondition("toString:"));
  }

  /**
   * Test {@link PropertyHelper#testIfCondition(Object)}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()}.</li>
   *   <li>When {@link Boolean#TRUE} toString.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#testIfCondition(Object)}
   */
  @Test
  public void testTestIfCondition_givenPropertyHelper_whenTrueToString_thenReturnTrue() {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();

    // Act and Assert
    assertTrue(propertyHelper.testIfCondition(Boolean.TRUE.toString()));
  }

  /**
   * Test {@link PropertyHelper#testIfCondition(Object)}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()}.</li>
   *   <li>When {@code true}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#testIfCondition(Object)}
   */
  @Test
  public void testTestIfCondition_givenPropertyHelper_whenTrue_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new PropertyHelper()).testIfCondition(true));
  }

  /**
   * Test {@link PropertyHelper#testIfCondition(Object)}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()}.</li>
   *   <li>When {@code Value}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#testIfCondition(Object)}
   */
  @Test
  public void testTestIfCondition_givenPropertyHelper_whenValue_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new PropertyHelper()).testIfCondition("Value"));
  }

  /**
   * Test {@link PropertyHelper#testIfCondition(Object)}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()}.</li>
   *   <li>When {@code yes}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#testIfCondition(Object)}
   */
  @Test
  public void testTestIfCondition_givenPropertyHelper_whenYes_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new PropertyHelper()).testIfCondition("yes"));
  }

  /**
   * Test {@link PropertyHelper#testUnlessCondition(Object)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>When {@code toString:}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#testUnlessCondition(Object)}
   */
  @Test
  public void testTestUnlessCondition_givenJavaLangObject_whenToString_thenReturnTrue() {
    // Arrange
    Project p = new Project();
    Class<Object> typeClass = Object.class;
    p.addDataTypeDefinition("on", typeClass);
    p.addBuildListener(new AntClassLoader());

    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.setProject(p);

    // Act and Assert
    assertTrue(propertyHelper.testUnlessCondition("toString:"));
  }

  /**
   * Test {@link PropertyHelper#testUnlessCondition(Object)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>When {@code toString:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#testUnlessCondition(Object)}
   */
  @Test
  public void testTestUnlessCondition_givenProjectAddBuildListenerAntClassLoader_whenToString() {
    // Arrange
    Project p = new Project();
    p.addBuildListener(new AntClassLoader());

    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.setProject(p);

    // Act and Assert
    assertTrue(propertyHelper.testUnlessCondition("toString:"));
  }

  /**
   * Test {@link PropertyHelper#testUnlessCondition(Object)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addReference empty string and {@code Value}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#testUnlessCondition(Object)}
   */
  @Test
  public void testTestUnlessCondition_givenProjectAddReferenceEmptyStringAndValue() {
    // Arrange
    Project p = new Project();
    p.addReference("", "Value");
    p.addBuildListener(new AntClassLoader());

    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.setProject(p);

    // Act and Assert
    assertFalse(propertyHelper.testUnlessCondition("toString:"));
  }

  /**
   * Test {@link PropertyHelper#testUnlessCondition(Object)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code on} and {@link Target#Target()}.</li>
   *   <li>When {@code toString:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#testUnlessCondition(Object)}
   */
  @Test
  public void testTestUnlessCondition_givenProjectAddTargetOnAndTarget_whenToString() throws BuildException {
    // Arrange
    Project p = new Project();
    p.addTarget("on", new Target());
    p.addBuildListener(new AntClassLoader());

    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.setProject(p);

    // Act and Assert
    assertTrue(propertyHelper.testUnlessCondition("toString:"));
  }

  /**
   * Test {@link PropertyHelper#testUnlessCondition(Object)}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()} add {@link Project} (default constructor).</li>
   *   <li>When {@code Value}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#testUnlessCondition(Object)}
   */
  @Test
  public void testTestUnlessCondition_givenPropertyHelperAddProject_whenValue_thenReturnTrue() {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.add(LocalProperties.get(new Project()));

    // Act and Assert
    assertTrue(propertyHelper.testUnlessCondition("Value"));
  }

  /**
   * Test {@link PropertyHelper#testUnlessCondition(Object)}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()} Project is {@link Project} (default constructor).</li>
   *   <li>When {@code ant.refid:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#testUnlessCondition(Object)}
   */
  @Test
  public void testTestUnlessCondition_givenPropertyHelperProjectIsProject_whenAntRefid() {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.setProject(new Project());

    // Act and Assert
    assertTrue(propertyHelper.testUnlessCondition("ant.refid:"));
  }

  /**
   * Test {@link PropertyHelper#testUnlessCondition(Object)}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()} Project is {@link Project} (default constructor).</li>
   *   <li>When {@code toString:}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#testUnlessCondition(Object)}
   */
  @Test
  public void testTestUnlessCondition_givenPropertyHelperProjectIsProject_whenToString() {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();
    propertyHelper.setProject(new Project());

    // Act and Assert
    assertTrue(propertyHelper.testUnlessCondition("toString:"));
  }

  /**
   * Test {@link PropertyHelper#testUnlessCondition(Object)}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()}.</li>
   *   <li>When {@code ant.refid:}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#testUnlessCondition(Object)}
   */
  @Test
  public void testTestUnlessCondition_givenPropertyHelper_whenAntRefid_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new PropertyHelper()).testUnlessCondition("ant.refid:"));
  }

  /**
   * Test {@link PropertyHelper#testUnlessCondition(Object)}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()}.</li>
   *   <li>When empty string.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#testUnlessCondition(Object)}
   */
  @Test
  public void testTestUnlessCondition_givenPropertyHelper_whenEmptyString_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new PropertyHelper()).testUnlessCondition(""));
  }

  /**
   * Test {@link PropertyHelper#testUnlessCondition(Object)}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()}.</li>
   *   <li>When {@link Boolean#FALSE} toString.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#testUnlessCondition(Object)}
   */
  @Test
  public void testTestUnlessCondition_givenPropertyHelper_whenFalseToString_thenReturnTrue() {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();

    // Act and Assert
    assertTrue(propertyHelper.testUnlessCondition(Boolean.FALSE.toString()));
  }

  /**
   * Test {@link PropertyHelper#testUnlessCondition(Object)}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()}.</li>
   *   <li>When forty-two.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#testUnlessCondition(Object)}
   */
  @Test
  public void testTestUnlessCondition_givenPropertyHelper_whenFortyTwo_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new PropertyHelper()).testUnlessCondition(42));
  }

  /**
   * Test {@link PropertyHelper#testUnlessCondition(Object)}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()}.</li>
   *   <li>When {@code no}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#testUnlessCondition(Object)}
   */
  @Test
  public void testTestUnlessCondition_givenPropertyHelper_whenNo_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new PropertyHelper()).testUnlessCondition("no"));
  }

  /**
   * Test {@link PropertyHelper#testUnlessCondition(Object)}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()}.</li>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#testUnlessCondition(Object)}
   */
  @Test
  public void testTestUnlessCondition_givenPropertyHelper_whenNull_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new PropertyHelper()).testUnlessCondition(null));
  }

  /**
   * Test {@link PropertyHelper#testUnlessCondition(Object)}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()}.</li>
   *   <li>When {@code off}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#testUnlessCondition(Object)}
   */
  @Test
  public void testTestUnlessCondition_givenPropertyHelper_whenOff_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new PropertyHelper()).testUnlessCondition("off"));
  }

  /**
   * Test {@link PropertyHelper#testUnlessCondition(Object)}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()}.</li>
   *   <li>When {@code on}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#testUnlessCondition(Object)}
   */
  @Test
  public void testTestUnlessCondition_givenPropertyHelper_whenOn_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new PropertyHelper()).testUnlessCondition("on"));
  }

  /**
   * Test {@link PropertyHelper#testUnlessCondition(Object)}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()}.</li>
   *   <li>When {@code toString:}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#testUnlessCondition(Object)}
   */
  @Test
  public void testTestUnlessCondition_givenPropertyHelper_whenToString_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new PropertyHelper()).testUnlessCondition("toString:"));
  }

  /**
   * Test {@link PropertyHelper#testUnlessCondition(Object)}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()}.</li>
   *   <li>When {@link Boolean#TRUE} toString.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#testUnlessCondition(Object)}
   */
  @Test
  public void testTestUnlessCondition_givenPropertyHelper_whenTrueToString_thenReturnFalse() {
    // Arrange
    PropertyHelper propertyHelper = new PropertyHelper();

    // Act and Assert
    assertFalse(propertyHelper.testUnlessCondition(Boolean.TRUE.toString()));
  }

  /**
   * Test {@link PropertyHelper#testUnlessCondition(Object)}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()}.</li>
   *   <li>When {@code true}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#testUnlessCondition(Object)}
   */
  @Test
  public void testTestUnlessCondition_givenPropertyHelper_whenTrue_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new PropertyHelper()).testUnlessCondition(true));
  }

  /**
   * Test {@link PropertyHelper#testUnlessCondition(Object)}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()}.</li>
   *   <li>When {@code Value}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#testUnlessCondition(Object)}
   */
  @Test
  public void testTestUnlessCondition_givenPropertyHelper_whenValue_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new PropertyHelper()).testUnlessCondition("Value"));
  }

  /**
   * Test {@link PropertyHelper#testUnlessCondition(Object)}.
   * <ul>
   *   <li>Given {@link PropertyHelper#PropertyHelper()}.</li>
   *   <li>When {@code yes}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PropertyHelper#testUnlessCondition(Object)}
   */
  @Test
  public void testTestUnlessCondition_givenPropertyHelper_whenYes_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new PropertyHelper()).testUnlessCondition("yes"));
  }
}
