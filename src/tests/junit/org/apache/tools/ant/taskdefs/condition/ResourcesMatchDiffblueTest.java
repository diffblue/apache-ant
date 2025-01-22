package org.apache.tools.ant.taskdefs.condition;

import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.Concat;
import org.apache.tools.ant.types.FileList;
import org.apache.tools.ant.types.FileList.FileName;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.ResourceCollection;
import org.apache.tools.ant.types.resources.Resources;
import org.junit.Test;

public class ResourcesMatchDiffblueTest {
  /**
   * Test {@link ResourcesMatch#add(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link ResourcesMatch} (default constructor).</li>
   *   <li>When {@link Resources#NONE}.</li>
   *   <li>Then {@link ResourcesMatch} (default constructor) eval.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourcesMatch#add(ResourceCollection)}
   */
  @Test
  public void testAdd_givenResourcesMatch_whenNone_thenResourcesMatchEval() throws BuildException {
    // Arrange
    ResourcesMatch resourcesMatch = new ResourcesMatch();

    // Act
    resourcesMatch.add(Resources.NONE);

    // Assert
    assertTrue(resourcesMatch.eval());
  }

  /**
   * Test {@link ResourcesMatch#eval()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) addFilelist {@link FileList#FileList()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourcesMatch#eval()}
   */
  @Test
  public void testEval_givenConcatAddFilelistFileList_thenReturnTrue() throws BuildException {
    // Arrange
    Concat rc = new Concat();
    rc.addFilelist(new FileList());

    ResourcesMatch resourcesMatch = new ResourcesMatch();
    resourcesMatch.add(rc);

    // Act and Assert
    assertTrue(resourcesMatch.eval());
  }

  /**
   * Test {@link ResourcesMatch#eval()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) addText {@code At least one resource must be provided, or some text.}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourcesMatch#eval()}
   */
  @Test
  public void testEval_givenConcatAddTextAtLeastOneResourceMustBeProvidedOrSomeText() throws BuildException {
    // Arrange
    Concat rc = new Concat();
    rc.addText("At least one resource must be provided, or some text.");

    ResourcesMatch resourcesMatch = new ResourcesMatch();
    resourcesMatch.add(rc);

    // Act and Assert
    assertTrue(resourcesMatch.eval());
  }

  /**
   * Test {@link ResourcesMatch#eval()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Dest is {@link Resource#Resource()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourcesMatch#eval()}
   */
  @Test
  public void testEval_givenConcatDestIsResource_thenReturnTrue() throws BuildException {
    // Arrange
    Concat rc = new Concat();
    rc.setDest(new Resource());
    rc.addFilelist(new FileList());

    ResourcesMatch resourcesMatch = new ResourcesMatch();
    resourcesMatch.add(rc);

    // Act and Assert
    assertTrue(resourcesMatch.eval());
  }

  /**
   * Test {@link ResourcesMatch#eval()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourcesMatch#eval()}
   */
  @Test
  public void testEval_givenConcatProjectIsProject_thenReturnTrue() throws BuildException {
    // Arrange
    Concat rc = new Concat();
    rc.setProject(new Project());
    rc.addText("At least one resource must be provided, or some text.");

    ResourcesMatch resourcesMatch = new ResourcesMatch();
    resourcesMatch.add(rc);

    // Act and Assert
    assertTrue(resourcesMatch.eval());
  }

  /**
   * Test {@link ResourcesMatch#eval()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) ResourceName is {@code concat (}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourcesMatch#eval()}
   */
  @Test
  public void testEval_givenConcatResourceNameIsConcat_thenReturnTrue() throws BuildException {
    // Arrange
    Concat rc = new Concat();
    rc.setResourceName("concat (");
    rc.addFilelist(new FileList());

    ResourcesMatch resourcesMatch = new ResourcesMatch();
    resourcesMatch.add(rc);

    // Act and Assert
    assertTrue(resourcesMatch.eval());
  }

  /**
   * Test {@link ResourcesMatch#eval()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code concat (}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourcesMatch#eval()}
   */
  @Test
  public void testEval_givenFileNameNameIsConcat_thenReturnTrue() throws BuildException {
    // Arrange
    FileName name = new FileName();
    name.setName("concat (");

    FileList list = new FileList();
    list.addConfiguredFile(name);

    Concat rc = new Concat();
    rc.addFilelist(list);

    ResourcesMatch resourcesMatch = new ResourcesMatch();
    resourcesMatch.add(rc);

    // Act and Assert
    assertTrue(resourcesMatch.eval());
  }

  /**
   * Test {@link ResourcesMatch#eval()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code ..}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourcesMatch#eval()}
   */
  @Test
  public void testEval_givenFileNameNameIsDotDot_thenReturnTrue() throws BuildException {
    // Arrange
    FileName name = new FileName();
    name.setName("..");

    FileList list = new FileList();
    list.addConfiguredFile(name);

    Concat rc = new Concat();
    rc.addFilelist(list);

    ResourcesMatch resourcesMatch = new ResourcesMatch();
    resourcesMatch.add(rc);

    // Act and Assert
    assertTrue(resourcesMatch.eval());
  }

  /**
   * Test {@link ResourcesMatch#eval()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code .}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourcesMatch#eval()}
   */
  @Test
  public void testEval_givenFileNameNameIsDot_thenReturnTrue() throws BuildException {
    // Arrange
    FileName name = new FileName();
    name.setName(".");

    FileList list = new FileList();
    list.addConfiguredFile(name);

    Concat rc = new Concat();
    rc.addFilelist(list);

    ResourcesMatch resourcesMatch = new ResourcesMatch();
    resourcesMatch.add(rc);

    // Act and Assert
    assertTrue(resourcesMatch.eval());
  }

  /**
   * Test {@link ResourcesMatch#eval()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is empty string.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourcesMatch#eval()}
   */
  @Test
  public void testEval_givenFileNameNameIsEmptyString_thenReturnTrue() throws BuildException {
    // Arrange
    FileName name = new FileName();
    name.setName("");

    FileList list = new FileList();
    list.addConfiguredFile(name);

    Concat rc = new Concat();
    rc.addFilelist(list);

    ResourcesMatch resourcesMatch = new ResourcesMatch();
    resourcesMatch.add(rc);

    // Act and Assert
    assertTrue(resourcesMatch.eval());
  }

  /**
   * Test {@link ResourcesMatch#eval()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code Name}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourcesMatch#eval()}
   */
  @Test
  public void testEval_givenFileNameNameIsName_thenReturnTrue() throws BuildException {
    // Arrange
    FileName name = new FileName();
    name.setName("Name");

    FileList list = new FileList();
    list.addConfiguredFile(name);

    Concat rc = new Concat();
    rc.setDest(new Resource());
    rc.addFilelist(list);

    ResourcesMatch resourcesMatch = new ResourcesMatch();
    resourcesMatch.add(rc);

    // Act and Assert
    assertTrue(resourcesMatch.eval());
  }

  /**
   * Test {@link ResourcesMatch#eval()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code Name}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourcesMatch#eval()}
   */
  @Test
  public void testEval_givenFileNameNameIsName_thenReturnTrue2() throws BuildException {
    // Arrange
    FileName name = new FileName();
    name.setName("Name");

    FileName name2 = new FileName();
    name2.setName("Name");

    FileList list = new FileList();
    list.addConfiguredFile(name2);
    list.addConfiguredFile(name);

    Concat rc = new Concat();
    rc.setDest(new Resource());
    rc.addFilelist(list);

    ResourcesMatch resourcesMatch = new ResourcesMatch();
    resourcesMatch.add(rc);

    // Act and Assert
    assertTrue(resourcesMatch.eval());
  }

  /**
   * Test {@link ResourcesMatch#eval()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourcesMatch#eval()}
   */
  @Test
  public void testEval_givenProjectAddBuildListenerAntClassLoader_thenReturnTrue() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Concat rc = new Concat();
    rc.setProject(project);
    rc.addText("At least one resource must be provided, or some text.");

    ResourcesMatch resourcesMatch = new ResourcesMatch();
    resourcesMatch.add(rc);

    // Act and Assert
    assertTrue(resourcesMatch.eval());
  }

  /**
   * Test {@link ResourcesMatch#eval()}.
   * <ul>
   *   <li>Given {@link ResourcesMatch} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourcesMatch#eval()}
   */
  @Test
  public void testEval_givenResourcesMatchAddNone_thenReturnTrue() throws BuildException {
    // Arrange
    ResourcesMatch resourcesMatch = new ResourcesMatch();
    resourcesMatch.add(Resources.NONE);

    // Act and Assert
    assertTrue(resourcesMatch.eval());
  }

  /**
   * Test {@link ResourcesMatch#eval()}.
   * <ul>
   *   <li>Given {@link ResourcesMatch} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourcesMatch#eval()}
   */
  @Test
  public void testEval_givenResourcesMatch_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new ResourcesMatch()).eval());
  }
}
