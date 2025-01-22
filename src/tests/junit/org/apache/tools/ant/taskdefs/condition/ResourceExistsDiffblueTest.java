package org.apache.tools.ant.taskdefs.condition;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.mappers.CutDirsMapper;
import org.apache.tools.ant.types.resources.FileResource;
import org.apache.tools.ant.types.resources.JavaResource;
import org.apache.tools.ant.types.resources.MappedResource;
import org.junit.Test;

public class ResourceExistsDiffblueTest {
  /**
   * Test {@link ResourceExists#add(Resource)}.
   * <ul>
   *   <li>Given {@link ResourceExists} (default constructor) add {@link Resource#Resource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceExists#add(Resource)}
   */
  @Test
  public void testAdd_givenResourceExistsAddResource_thenThrowBuildException() {
    // Arrange
    ResourceExists resourceExists = new ResourceExists();
    resourceExists.add(new Resource());

    // Act and Assert
    assertThrows(BuildException.class, () -> resourceExists.add(new Resource()));
  }

  /**
   * Test {@link ResourceExists#validate()}.
   * <ul>
   *   <li>Given {@link ResourceExists} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceExists#validate()}
   */
  @Test
  public void testValidate_givenResourceExists_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new ResourceExists()).validate());
  }

  /**
   * Test {@link ResourceExists#eval()}.
   * <p>
   * Method under test: {@link ResourceExists#eval()}
   */
  @Test
  public void testEval() throws BuildException {
    // Arrange
    ResourceExists resourceExists = new ResourceExists();
    resourceExists.add(new Resource("Name", true, 1L));

    // Act and Assert
    assertTrue(resourceExists.eval());
  }

  /**
   * Test {@link ResourceExists#eval()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is {@code file attribute is null!}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceExists#eval()}
   */
  @Test
  public void testEval_givenFileResourceNameIsFileAttributeIsNull_thenReturnFalse() throws BuildException {
    // Arrange
    FileResource r = new FileResource();
    r.setName("file attribute is null!");

    ResourceExists resourceExists = new ResourceExists();
    resourceExists.add(r);

    // Act and Assert
    assertFalse(resourceExists.eval());
  }

  /**
   * Test {@link ResourceExists#eval()}.
   * <ul>
   *   <li>Given {@link JavaResource#JavaResource(String, Path)} with {@code Name} and path is {@link Path#Path(Project)} Project is {@link Project} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceExists#eval()}
   */
  @Test
  public void testEval_givenJavaResourceWithNameAndPathIsPathProjectIsProject_thenReturnFalse() throws BuildException {
    // Arrange
    JavaResource r = new JavaResource("Name", new Path(new Project()));
    r.setProject(new Project());

    ResourceExists resourceExists = new ResourceExists();
    resourceExists.add(r);

    // Act and Assert
    assertFalse(resourceExists.eval());
  }

  /**
   * Test {@link ResourceExists#eval()}.
   * <ul>
   *   <li>Given {@link JavaResource#JavaResource(String, Path)} with {@code Name} and path is {@link Path#Path(Project, String)} Project is {@link Project} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceExists#eval()}
   */
  @Test
  public void testEval_givenJavaResourceWithNameAndPathIsPathProjectIsProject_thenReturnFalse2() throws BuildException {
    // Arrange
    Path path = new Path(null, "*");
    path.setProject(null);

    JavaResource r = new JavaResource("Name", path);
    r.setProject(new Project());

    ResourceExists resourceExists = new ResourceExists();
    resourceExists.add(r);

    // Act and Assert
    assertFalse(resourceExists.eval());
  }

  /**
   * Test {@link ResourceExists#eval()}.
   * <ul>
   *   <li>Given {@link JavaResource#JavaResource(String, Path)} with name is {@code .} and path is {@link Path#Path(Project, String)} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceExists#eval()}
   */
  @Test
  public void testEval_givenJavaResourceWithNameIsDotAndPathIsPathProjectIsProject() throws BuildException {
    // Arrange
    Path path = new Path(null, "*");
    path.setProject(null);

    JavaResource r = new JavaResource(".", path);
    r.setProject(new Project());

    ResourceExists resourceExists = new ResourceExists();
    resourceExists.add(r);

    // Act and Assert
    assertTrue(resourceExists.eval());
  }

  /**
   * Test {@link ResourceExists#eval()}.
   * <ul>
   *   <li>Given {@link ResourceExists} (default constructor) add {@link MappedResource#MappedResource(Resource, FileNameMapper)} with r is {@link Resource#Resource()} and m is {@link CutDirsMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceExists#eval()}
   */
  @Test
  public void testEval_givenResourceExistsAddMappedResourceWithRIsResourceAndMIsCutDirsMapper() throws BuildException {
    // Arrange
    ResourceExists resourceExists = new ResourceExists();
    Resource r = new Resource();
    resourceExists.add(new MappedResource(r, new CutDirsMapper()));

    // Act and Assert
    assertTrue(resourceExists.eval());
  }

  /**
   * Test {@link ResourceExists#eval()}.
   * <ul>
   *   <li>Given {@link ResourceExists} (default constructor) add {@link Resource#Resource(String)} with {@code Name}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceExists#eval()}
   */
  @Test
  public void testEval_givenResourceExistsAddResourceWithName_thenReturnFalse() throws BuildException {
    // Arrange
    ResourceExists resourceExists = new ResourceExists();
    resourceExists.add(new Resource("Name"));

    // Act and Assert
    assertFalse(resourceExists.eval());
  }

  /**
   * Test {@link ResourceExists#eval()}.
   * <ul>
   *   <li>Given {@link ResourceExists} (default constructor) add {@link Resource#Resource()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceExists#eval()}
   */
  @Test
  public void testEval_givenResourceExistsAddResource_thenReturnTrue() throws BuildException {
    // Arrange
    ResourceExists resourceExists = new ResourceExists();
    resourceExists.add(new Resource());

    // Act and Assert
    assertTrue(resourceExists.eval());
  }

  /**
   * Test {@link ResourceExists#eval()}.
   * <ul>
   *   <li>Given {@link ResourceExists} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceExists#eval()}
   */
  @Test
  public void testEval_givenResourceExists_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new ResourceExists()).eval());
  }

  /**
   * Test new {@link ResourceExists} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link ResourceExists}
   */
  @Test
  public void testNewResourceExists() {
    // Arrange and Act
    ResourceExists actualResourceExists = new ResourceExists();

    // Assert
    Location location = actualResourceExists.getLocation();
    assertNull(location.getFileName());
    assertNull(actualResourceExists.getDescription());
    assertNull(actualResourceExists.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }
}
