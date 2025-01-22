package org.apache.tools.ant.types.resources.selectors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.Resource;
import org.junit.Test;

public class InstanceOfDiffblueTest {
  /**
   * Test {@link InstanceOf#setClass(Class)}.
   * <ul>
   *   <li>Given {@link InstanceOf} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link InstanceOf#setClass(Class)}
   */
  @Test
  public void testSetClass_givenInstanceOfProjectIsProject_thenThrowBuildException() {
    // Arrange
    InstanceOf instanceOf = new InstanceOf();
    instanceOf.setProject(new Project());
    instanceOf.setType("foo");
    instanceOf.setURI("foo");
    Class<Object> c = Object.class;
    instanceOf.setClass(c);
    Class<Object> c2 = Object.class;

    // Act and Assert
    assertThrows(BuildException.class, () -> instanceOf.setClass(c2));
  }

  /**
   * Test {@link InstanceOf#setClass(Class)}.
   * <ul>
   *   <li>Given {@link InstanceOf} (default constructor).</li>
   *   <li>Then {@link InstanceOf} (default constructor) CheckClass is {@link Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link InstanceOf#setClass(Class)}
   */
  @Test
  public void testSetClass_givenInstanceOf_thenInstanceOfCheckClassIsObject() {
    // Arrange
    InstanceOf instanceOf = new InstanceOf();
    Class<Object> c = Object.class;

    // Act
    instanceOf.setClass(c);

    // Assert
    Class<Object> expectedCheckClass = Object.class;
    assertEquals(expectedCheckClass, instanceOf.getCheckClass());
  }

  /**
   * Test {@link InstanceOf#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link InstanceOf} (default constructor) Class is {@link Object}.</li>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link InstanceOf#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenInstanceOfClassIsObject_whenResource_thenReturnTrue() {
    // Arrange
    InstanceOf instanceOf = new InstanceOf();
    Class<Object> c = Object.class;
    instanceOf.setClass(c);

    // Act and Assert
    assertTrue(instanceOf.isSelected(new Resource()));
  }

  /**
   * Test {@link InstanceOf#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link InstanceOf} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link InstanceOf#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenInstanceOfProjectIsProject_thenThrowBuildException() {
    // Arrange
    InstanceOf instanceOf = new InstanceOf();
    instanceOf.setProject(new Project());
    instanceOf.setType("Exactly one of class|type must be set.");

    // Act and Assert
    assertThrows(BuildException.class, () -> instanceOf.isSelected(new Resource()));
  }

  /**
   * Test {@link InstanceOf#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link InstanceOf} (default constructor).</li>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link InstanceOf#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenInstanceOf_whenResource_thenThrowBuildException() {
    // Arrange
    InstanceOf instanceOf = new InstanceOf();

    // Act and Assert
    assertThrows(BuildException.class, () -> instanceOf.isSelected(new Resource()));
  }

  /**
   * Test {@link InstanceOf#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link InstanceOf#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project p = new Project();
    p.addBuildListener(new AntClassLoader());

    InstanceOf instanceOf = new InstanceOf();
    instanceOf.setProject(p);
    instanceOf.setType("Exactly one of class|type must be set.");

    // Act and Assert
    assertThrows(BuildException.class, () -> instanceOf.isSelected(new Resource()));
  }

  /**
   * Test {@link InstanceOf#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) CoreLoader is {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link InstanceOf#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenProjectCoreLoaderIsAntClassLoader_thenThrowBuildException() {
    // Arrange
    Project p = new Project();
    p.setCoreLoader(new AntClassLoader());
    p.addBuildListener(new AntClassLoader());

    InstanceOf instanceOf = new InstanceOf();
    instanceOf.setProject(p);
    instanceOf.setType("Exactly one of class|type must be set.");

    // Act and Assert
    assertThrows(BuildException.class, () -> instanceOf.isSelected(new Resource()));
  }

  /**
   * Test {@link InstanceOf#isSelected(Resource)}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link InstanceOf#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_thenThrowBuildException() {
    // Arrange
    InstanceOf instanceOf = new InstanceOf();
    instanceOf.setType("Exactly one of class|type must be set.");

    // Act and Assert
    assertThrows(BuildException.class, () -> instanceOf.isSelected(new Resource()));
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link InstanceOf}
   *   <li>{@link InstanceOf#setProject(Project)}
   *   <li>{@link InstanceOf#setType(String)}
   *   <li>{@link InstanceOf#setURI(String)}
   *   <li>{@link InstanceOf#getCheckClass()}
   *   <li>{@link InstanceOf#getType()}
   *   <li>{@link InstanceOf#getURI()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    InstanceOf actualInstanceOf = new InstanceOf();
    actualInstanceOf.setProject(new Project());
    actualInstanceOf.setType("foo");
    actualInstanceOf.setURI("foo");
    Class<?> actualCheckClass = actualInstanceOf.getCheckClass();
    String actualType = actualInstanceOf.getType();

    // Assert
    assertEquals("foo", actualType);
    assertEquals("foo", actualInstanceOf.getURI());
    assertNull(actualCheckClass);
  }
}
