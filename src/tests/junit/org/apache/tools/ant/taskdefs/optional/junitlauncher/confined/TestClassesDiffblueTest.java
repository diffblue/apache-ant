package org.apache.tools.ant.taskdefs.optional.junitlauncher.confined;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.util.List;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.Concat;
import org.apache.tools.ant.types.FileList;
import org.apache.tools.ant.types.FileList.FileName;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.resources.Resources;
import org.junit.Test;

public class TestClassesDiffblueTest {
  /**
   * Test new {@link TestClasses} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link TestClasses}
   */
  @Test
  public void testNewTestClasses() {
    // Arrange and Act
    TestClasses actualTestClasses = new TestClasses();

    // Assert
    assertNull(actualTestClasses.getOutputDir());
    assertNull(actualTestClasses.getHaltOnFailure());
    assertNull(actualTestClasses.getFailureProperty());
    assertNull(actualTestClasses.getIfProperty());
    assertNull(actualTestClasses.getUnlessProperty());
    assertNull(actualTestClasses.excludeEngines);
    assertNull(actualTestClasses.includeEngines);
    assertNull(actualTestClasses.getForkDefinition());
    assertEquals(0, actualTestClasses.getExcludeEngines().length);
    assertEquals(0, actualTestClasses.getIncludeEngines().length);
    assertTrue(actualTestClasses.getTestClassNames().isEmpty());
    assertTrue(actualTestClasses.getListeners().isEmpty());
    assertTrue(actualTestClasses.listeners.isEmpty());
  }

  /**
   * Test {@link TestClasses#getTestClassNames()}.
   * <p>
   * Method under test: {@link TestClasses#getTestClassNames()}
   */
  @Test
  public void testGetTestClassNames() {
    // Arrange
    TestClasses testClasses = new TestClasses();
    testClasses.add(new Resource(".", true, 256L));

    // Act and Assert
    assertTrue(testClasses.getTestClassNames().isEmpty());
  }

  /**
   * Test {@link TestClasses#getTestClassNames()}.
   * <p>
   * Method under test: {@link TestClasses#getTestClassNames()}
   */
  @Test
  public void testGetTestClassNames2() {
    // Arrange
    Concat resourceCollection = new Concat();
    resourceCollection.setDest(new Resource("No directory specified for %s."));
    resourceCollection.addFilelist(new FileList());

    TestClasses testClasses = new TestClasses();
    testClasses.add(resourceCollection);

    // Act and Assert
    assertTrue(testClasses.getTestClassNames().isEmpty());
  }

  /**
   * Test {@link TestClasses#getTestClassNames()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) addFilelist {@link FileList#FileList()}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link TestClasses#getTestClassNames()}
   */
  @Test
  public void testGetTestClassNames_givenConcatAddFilelistFileList_thenReturnEmpty() {
    // Arrange
    Concat resourceCollection = new Concat();
    resourceCollection.addFilelist(new FileList());

    TestClasses testClasses = new TestClasses();
    testClasses.add(resourceCollection);

    // Act and Assert
    assertTrue(testClasses.getTestClassNames().isEmpty());
  }

  /**
   * Test {@link TestClasses#getTestClassNames()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) addText {@code Text}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link TestClasses#getTestClassNames()}
   */
  @Test
  public void testGetTestClassNames_givenConcatAddTextText_thenReturnEmpty() {
    // Arrange
    Concat resourceCollection = new Concat();
    resourceCollection.addText("Text");

    TestClasses testClasses = new TestClasses();
    testClasses.add(resourceCollection);

    // Act and Assert
    assertTrue(testClasses.getTestClassNames().isEmpty());
  }

  /**
   * Test {@link TestClasses#getTestClassNames()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Dest is {@link Resource#Resource()}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link TestClasses#getTestClassNames()}
   */
  @Test
  public void testGetTestClassNames_givenConcatDestIsResource_thenReturnEmpty() {
    // Arrange
    Concat resourceCollection = new Concat();
    resourceCollection.setDest(new Resource());
    resourceCollection.addFilelist(new FileList());

    TestClasses testClasses = new TestClasses();
    testClasses.add(resourceCollection);

    // Act and Assert
    assertTrue(testClasses.getTestClassNames().isEmpty());
  }

  /**
   * Test {@link TestClasses#getTestClassNames()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link TestClasses#getTestClassNames()}
   */
  @Test
  public void testGetTestClassNames_givenConcatProjectIsProject_thenReturnEmpty() {
    // Arrange
    Concat resourceCollection = new Concat();
    resourceCollection.setProject(new Project());
    resourceCollection.addText("Text");

    TestClasses testClasses = new TestClasses();
    testClasses.add(resourceCollection);

    // Act and Assert
    assertTrue(testClasses.getTestClassNames().isEmpty());
  }

  /**
   * Test {@link TestClasses#getTestClassNames()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) ResourceName is {@code concat (}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link TestClasses#getTestClassNames()}
   */
  @Test
  public void testGetTestClassNames_givenConcatResourceNameIsConcat_thenReturnEmpty() {
    // Arrange
    Concat resourceCollection = new Concat();
    resourceCollection.setResourceName("concat (");
    resourceCollection.addFilelist(new FileList());

    TestClasses testClasses = new TestClasses();
    testClasses.add(resourceCollection);

    // Act and Assert
    assertTrue(testClasses.getTestClassNames().isEmpty());
  }

  /**
   * Test {@link TestClasses#getTestClassNames()}.
   * <ul>
   *   <li>Given {@link FileList.FileName} (default constructor) Name is {@code concat (}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link TestClasses#getTestClassNames()}
   */
  @Test
  public void testGetTestClassNames_givenFileNameNameIsConcat_thenReturnEmpty() {
    // Arrange
    FileName name = new FileName();
    name.setName("concat (");

    FileList list = new FileList();
    list.addConfiguredFile(name);

    Concat resourceCollection = new Concat();
    resourceCollection.addFilelist(list);

    TestClasses testClasses = new TestClasses();
    testClasses.add(resourceCollection);

    // Act and Assert
    assertTrue(testClasses.getTestClassNames().isEmpty());
  }

  /**
   * Test {@link TestClasses#getTestClassNames()}.
   * <ul>
   *   <li>Given {@link TestClasses} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link TestClasses#getTestClassNames()}
   */
  @Test
  public void testGetTestClassNames_givenTestClassesAddNone_thenReturnEmpty() {
    // Arrange
    TestClasses testClasses = new TestClasses();
    testClasses.add(Resources.NONE);

    // Act and Assert
    assertTrue(testClasses.getTestClassNames().isEmpty());
  }

  /**
   * Test {@link TestClasses#getTestClassNames()}.
   * <ul>
   *   <li>Given {@link TestClasses} (default constructor) add {@link Resource#Resource(String)} with name is {@code .}.</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link TestClasses#getTestClassNames()}
   */
  @Test
  public void testGetTestClassNames_givenTestClassesAddResourceWithNameIsDot_thenReturnEmpty() {
    // Arrange
    TestClasses testClasses = new TestClasses();
    testClasses.add(new Resource("."));

    // Act and Assert
    assertTrue(testClasses.getTestClassNames().isEmpty());
  }

  /**
   * Test {@link TestClasses#getTestClassNames()}.
   * <ul>
   *   <li>Given {@link TestClasses} (default constructor).</li>
   *   <li>Then return Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link TestClasses#getTestClassNames()}
   */
  @Test
  public void testGetTestClassNames_givenTestClasses_thenReturnEmpty() {
    // Arrange, Act and Assert
    assertTrue((new TestClasses()).getTestClassNames().isEmpty());
  }

  /**
   * Test {@link TestClasses#getTestClassNames()}.
   * <ul>
   *   <li>Then return size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link TestClasses#getTestClassNames()}
   */
  @Test
  public void testGetTestClassNames_thenReturnSizeIsOne() {
    // Arrange
    TestClasses testClasses = new TestClasses();
    testClasses.add(new Resource(".class", true, 256L));

    // Act
    List<String> actualTestClassNames = testClasses.getTestClassNames();

    // Assert
    assertEquals(1, actualTestClassNames.size());
    assertEquals("", actualTestClassNames.get(0));
  }

  /**
   * Test {@link TestClasses#toForkedRepresentations()}.
   * <p>
   * Method under test: {@link TestClasses#toForkedRepresentations()}
   */
  @Test
  public void testToForkedRepresentations() throws IllegalStateException {
    // Arrange, Act and Assert
    assertThrows(IllegalStateException.class, () -> (new TestClasses()).toForkedRepresentations());
  }
}
