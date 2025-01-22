package org.apache.tools.ant.types.resources.selectors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.ResourceCollection;
import org.apache.tools.ant.types.resources.Union;
import org.junit.Test;

public class CompareDiffblueTest {
  /**
   * Test {@link Compare#createControl()}.
   * <p>
   * Method under test: {@link Compare#createControl()}
   */
  @Test
  public void testCreateControl() {
    // Arrange and Act
    ResourceCollection actualCreateControlResult = (new Compare()).createControl();

    // Assert
    assertTrue(actualCreateControlResult instanceof Union);
    Location location = ((Union) actualCreateControlResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((Union) actualCreateControlResult).getDescription());
    assertNull(((Union) actualCreateControlResult).getProject());
    assertNull(((Union) actualCreateControlResult).getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualCreateControlResult.size());
    assertFalse(((Union) actualCreateControlResult).isReference());
    assertTrue(((Union) actualCreateControlResult).getResourceCollections().isEmpty());
    assertTrue(actualCreateControlResult.isEmpty());
    assertTrue(((Union) actualCreateControlResult).isCache());
  }

  /**
   * Test {@link Compare#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link Compare} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Compare#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenCompare() {
    // Arrange
    Compare compare = new Compare();

    // Act and Assert
    assertThrows(BuildException.class, () -> compare.isSelected(new Resource()));
  }

  /**
   * Test {@link Compare#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link Compare} (default constructor) Description is {@code The characteristics of someone or something}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Compare#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenCompareDescriptionIsTheCharacteristicsOfSomeoneOrSomething() {
    // Arrange
    Compare compare = new Compare();
    compare.setDescription("The characteristics of someone or something");

    // Act and Assert
    assertThrows(BuildException.class, () -> compare.isSelected(new Resource()));
  }

  /**
   * Test {@link Compare#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link Compare} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Compare#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenCompareProjectIsProject() {
    // Arrange
    Compare compare = new Compare();
    compare.setProject(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> compare.isSelected(new Resource()));
  }

  /**
   * Test {@link Compare#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Compare#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenJavaLangObject() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("%s the <control> element should be specified exactly once.", typeClass);

    Compare compare = new Compare();
    compare.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> compare.isSelected(new Resource()));
  }

  /**
   * Test {@link Compare#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@code Compare}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Compare#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenOrgApacheToolsAntTypesResourcesSelectorsCompare() {
    // Arrange
    Project project = new Project();
    Class<Compare> typeClass = Compare.class;
    project.addDataTypeDefinition("%s the <control> element should be specified exactly once.", typeClass);

    Compare compare = new Compare();
    compare.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> compare.isSelected(new Resource()));
  }

  /**
   * Test new {@link Compare} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Compare}
   */
  @Test
  public void testNewCompare() {
    // Arrange and Act
    Compare actualCompare = new Compare();

    // Assert
    Location location = actualCompare.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCompare.getDescription());
    assertNull(actualCompare.getProject());
    assertNull(actualCompare.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualCompare.isReference());
  }
}
