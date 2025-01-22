package org.apache.tools.ant.property;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.PropertyHelper;
import org.junit.Test;

public class LocalPropertiesDiffblueTest {
  /**
   * Test {@link LocalProperties#initialValue()}.
   * <p>
   * Method under test: {@link LocalProperties#initialValue()}
   */
  @Test
  public void testInitialValue() {
    // Arrange, Act and Assert
    assertTrue(LocalProperties.get(new Project()).initialValue().getPropertyNames().isEmpty());
  }

  /**
   * Test {@link LocalProperties#evaluate(String, PropertyHelper)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link LocalProperties#evaluate(String, PropertyHelper)}
   */
  @Test
  public void testEvaluate_givenProject() {
    // Arrange, Act and Assert
    assertNull(LocalProperties.get(new Project()).evaluate("Property", null));
  }

  /**
   * Test {@link LocalProperties#evaluate(String, PropertyHelper)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addLocal {@code Property}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LocalProperties#evaluate(String, PropertyHelper)}
   */
  @Test
  public void testEvaluate_givenProjectAddLocalProperty() {
    // Arrange
    LocalProperties getResult = LocalProperties.get(new Project());
    getResult.addLocal("Property");

    // Act and Assert
    assertNull(getResult.evaluate("Property", null));
  }

  /**
   * Test {@link LocalProperties#setNew(String, Object, PropertyHelper)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link LocalProperties#setNew(String, Object, PropertyHelper)}
   */
  @Test
  public void testSetNew_givenProject() {
    // Arrange, Act and Assert
    assertFalse(LocalProperties.get(new Project()).setNew("Property", "Value", null));
  }

  /**
   * Test {@link LocalProperties#setNew(String, Object, PropertyHelper)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addLocal {@code Property}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LocalProperties#setNew(String, Object, PropertyHelper)}
   */
  @Test
  public void testSetNew_givenProjectAddLocalProperty() {
    // Arrange
    LocalProperties getResult = LocalProperties.get(new Project());
    getResult.addLocal("Property");

    // Act and Assert
    assertFalse(getResult.setNew("Property", "Value", null));
  }

  /**
   * Test {@link LocalProperties#set(String, Object, PropertyHelper)} with {@code String}, {@code Object}, {@code PropertyHelper}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link LocalProperties#set(String, Object, PropertyHelper)}
   */
  @Test
  public void testSetWithStringObjectPropertyHelper_givenProject() {
    // Arrange, Act and Assert
    assertFalse(LocalProperties.get(new Project()).set("Property", "Value", null));
  }

  /**
   * Test {@link LocalProperties#set(String, Object, PropertyHelper)} with {@code String}, {@code Object}, {@code PropertyHelper}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addLocal {@code Property}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LocalProperties#set(String, Object, PropertyHelper)}
   */
  @Test
  public void testSetWithStringObjectPropertyHelper_givenProjectAddLocalProperty() {
    // Arrange
    LocalProperties getResult = LocalProperties.get(new Project());
    getResult.addLocal("Property");

    // Act and Assert
    assertFalse(getResult.set("Property", "Value", null));
  }

  /**
   * Test {@link LocalProperties#getPropertyNames()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link LocalProperties#getPropertyNames()}
   */
  @Test
  public void testGetPropertyNames_givenProject() {
    // Arrange, Act and Assert
    assertTrue(LocalProperties.get(new Project()).getPropertyNames().isEmpty());
  }

  /**
   * Test {@link LocalProperties#getPropertyNames()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addLocal {@code Property}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LocalProperties#getPropertyNames()}
   */
  @Test
  public void testGetPropertyNames_givenProjectAddLocalProperty() {
    // Arrange
    LocalProperties getResult = LocalProperties.get(new Project());
    getResult.addLocal("Property");

    // Act and Assert
    assertTrue(getResult.getPropertyNames().isEmpty());
  }
}
