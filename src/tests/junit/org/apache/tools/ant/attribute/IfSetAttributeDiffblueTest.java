package org.apache.tools.ant.attribute;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Target;
import org.apache.tools.ant.UnknownElement;
import org.apache.tools.ant.attribute.IfSetAttribute.Unless;
import org.junit.Test;

public class IfSetAttributeDiffblueTest {
  /**
   * Test {@link IfSetAttribute#isEnabled(UnknownElement, String)}.
   * <ul>
   *   <li>Given {@link IfSetAttribute} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>When {@code 42}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IfSetAttribute#isEnabled(UnknownElement, String)}
   */
  @Test
  public void testIsEnabled_givenIfSetAttributeProjectIsProject_when42_thenReturnFalse() {
    // Arrange
    IfSetAttribute ifSetAttribute = new IfSetAttribute();
    ifSetAttribute.setProject(new Project());

    // Act and Assert
    assertFalse(ifSetAttribute.isEnabled(new UnknownElement("Element Name"), "42"));
  }

  /**
   * Test {@link IfSetAttribute#isEnabled(UnknownElement, String)}.
   * <ul>
   *   <li>Given {@link IfSetAttribute} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IfSetAttribute#isEnabled(UnknownElement, String)}
   */
  @Test
  public void testIsEnabled_givenIfSetAttributeProjectIsProject_whenNull_thenReturnFalse() {
    // Arrange
    IfSetAttribute ifSetAttribute = new IfSetAttribute();
    ifSetAttribute.setProject(new Project());

    // Act and Assert
    assertFalse(ifSetAttribute.isEnabled(new UnknownElement("Element Name"), null));
  }

  /**
   * Test {@link IfSetAttribute#isEnabled(UnknownElement, String)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IfSetAttribute#isEnabled(UnknownElement, String)}
   */
  @Test
  public void testIsEnabled_givenJavaLangObject_whenNull_thenReturnFalse() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Adding reference: ", typeClass);
    project.addBuildListener(new AntClassLoader());

    IfSetAttribute ifSetAttribute = new IfSetAttribute();
    ifSetAttribute.setProject(project);

    // Act and Assert
    assertFalse(ifSetAttribute.isEnabled(new UnknownElement("Element Name"), null));
  }

  /**
   * Test {@link IfSetAttribute#isEnabled(UnknownElement, String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IfSetAttribute#isEnabled(UnknownElement, String)}
   */
  @Test
  public void testIsEnabled_givenProjectAddBuildListenerAntClassLoader_whenNull() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    IfSetAttribute ifSetAttribute = new IfSetAttribute();
    ifSetAttribute.setProject(project);

    // Act and Assert
    assertFalse(ifSetAttribute.isEnabled(new UnknownElement("Element Name"), null));
  }

  /**
   * Test {@link IfSetAttribute#isEnabled(UnknownElement, String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code Adding reference:} and {@link Target#Target()}.</li>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IfSetAttribute#isEnabled(UnknownElement, String)}
   */
  @Test
  public void testIsEnabled_givenProjectAddTargetAddingReferenceAndTarget_whenNull() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("Adding reference: ", new Target());
    project.addBuildListener(new AntClassLoader());

    IfSetAttribute ifSetAttribute = new IfSetAttribute();
    ifSetAttribute.setProject(project);

    // Act and Assert
    assertFalse(ifSetAttribute.isEnabled(new UnknownElement("Element Name"), null));
  }

  /**
   * Test new {@link IfSetAttribute} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link IfSetAttribute}
   */
  @Test
  public void testNewIfSetAttribute() {
    // Arrange and Act
    IfSetAttribute actualIfSetAttribute = new IfSetAttribute();

    // Assert
    Location location = actualIfSetAttribute.getLocation();
    assertNull(location.getFileName());
    assertNull(actualIfSetAttribute.getDescription());
    assertNull(actualIfSetAttribute.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(actualIfSetAttribute.isPositive());
  }

  /**
   * Test Unless new {@link Unless} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Unless}
   */
  @Test
  public void testUnlessNewUnless() {
    // Arrange and Act
    Unless actualUnless = new Unless();

    // Assert
    Location location = actualUnless.getLocation();
    assertNull(location.getFileName());
    assertNull(actualUnless.getDescription());
    assertNull(actualUnless.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualUnless.isPositive());
  }
}
