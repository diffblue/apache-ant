package org.apache.tools.ant.types.resources.selectors;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.nio.file.Paths;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.resources.FileResource;
import org.junit.Test;

public class NameDiffblueTest {
  /**
   * Test {@link Name#setRegex(String)}.
   * <p>
   * Method under test: {@link Name#setRegex(String)}
   */
  @Test
  public void testSetRegex() {
    // Arrange
    Name name = new Name();

    // Act
    name.setRegex("foo");

    // Assert
    assertEquals("foo", name.getRegex());
  }

  /**
   * Test {@link Name#doesHandledirSep()}.
   * <ul>
   *   <li>Given {@link Name} (default constructor) HandleDirSep is {@code true}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Name#doesHandledirSep()}
   */
  @Test
  public void testDoesHandledirSep_givenNameHandleDirSepIsTrue_thenReturnTrue() {
    // Arrange
    Name name = new Name();
    name.setHandleDirSep(true);

    // Act and Assert
    assertTrue(name.doesHandledirSep());
  }

  /**
   * Test {@link Name#doesHandledirSep()}.
   * <ul>
   *   <li>Given {@link Name} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Name#doesHandledirSep()}
   */
  @Test
  public void testDoesHandledirSep_givenName_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new Name()).doesHandledirSep());
  }

  /**
   * Test {@link Name#isSelected(Resource)}.
   * <p>
   * Method under test: {@link Name#isSelected(Resource)}
   */
  @Test
  public void testIsSelected() {
    // Arrange
    Name name = new Name();
    name.setName(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString());
    name.setProject(null);

    // Act and Assert
    assertTrue(name.isSelected(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile())));
  }

  /**
   * Test {@link Name#isSelected(Resource)}.
   * <p>
   * Method under test: {@link Name#isSelected(Resource)}
   */
  @Test
  public void testIsSelected2() {
    // Arrange
    Name name = new Name();
    name.setName("ant.regexp.regexpimpl");
    name.setProject(null);

    FileResource r = new FileResource();
    r.setName(String.join("", Paths.get(System.getProperty("user.dir"), "file").toString(), " attribute is null!"));

    // Act and Assert
    assertFalse(name.isSelected(r));
  }

  /**
   * Test {@link Name#isSelected(Resource)}.
   * <ul>
   *   <li>Given empty string.</li>
   *   <li>When {@link FileResource#FileResource()} Name is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Name#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenEmptyString_whenFileResourceNameIsEmptyString() {
    // Arrange
    Name name = new Name();
    name.setName("ant.regexp.regexpimpl");
    name.setProject(null);

    FileResource r = new FileResource();
    r.setName("");

    // Act and Assert
    assertFalse(name.isSelected(r));
  }

  /**
   * Test {@link Name#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@code file attribute is null!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Name#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenFileAttributeIsNull() {
    // Arrange
    Name name = new Name();
    name.setName("ant.regexp.regexpimpl");
    name.setProject(null);

    FileResource r = new FileResource();
    r.setName("file attribute is null!");

    // Act and Assert
    assertFalse(name.isSelected(r));
  }

  /**
   * Test {@link Name#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link Name} (default constructor) Name is {@code Name}.</li>
   *   <li>When {@link Resource#Resource(String)} with {@code Name}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Name#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenNameNameIsName_whenResourceWithName_thenReturnTrue() {
    // Arrange
    Name name = new Name();
    name.setName("Name");
    name.setProject(null);

    // Act and Assert
    assertTrue(name.isSelected(new Resource("Name")));
  }

  /**
   * Test {@link Name#isSelected(Resource)}.
   * <ul>
   *   <li>Given Property is {@code java.io.tmpdir} is array of {@link String} with {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link Name#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenPropertyIsJavaIoTmpdirIsArrayOfStringWithTestTxtToFile() {
    // Arrange
    Name name = new Name();
    name.setName("ant.regexp.regexpimpl");
    name.setProject(null);

    FileResource r = new FileResource();
    r.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    r.setName(String.join("", Paths.get(System.getProperty("user.dir"), "file").toString(), " attribute is null!"));

    // Act and Assert
    assertFalse(name.isSelected(r));
  }

  /**
   * Test {@link Name#isSelected(Resource)}.
   * <ul>
   *   <li>Given Property is {@code java.io.tmpdir} is array of {@link String} with {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link Name#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenPropertyIsJavaIoTmpdirIsArrayOfStringWithTestTxtToFile2() {
    // Arrange
    Name name = new Name();
    name.setName("ant.regexp.regexpimpl");
    name.setProject(null);

    FileResource r = new FileResource();
    r.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    r.setName("");

    // Act and Assert
    assertFalse(name.isSelected(r));
  }

  /**
   * Test {@link Name#isSelected(Resource)}.
   * <ul>
   *   <li>When {@link FileResource#FileResource(File)} with f is Property is {@code java.io.tmpdir} is {@code ..} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link Name#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_whenFileResourceWithFIsPropertyIsJavaIoTmpdirIsDotDotToFile() {
    // Arrange
    Name name = new Name();
    name.setName("ant.regexp.regexpimpl");
    name.setProject(null);

    // Act and Assert
    assertFalse(name.isSelected(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "..").toFile())));
  }

  /**
   * Test {@link Name#isSelected(Resource)}.
   * <ul>
   *   <li>When {@link FileResource#FileResource(File)} with f is Property is {@code java.io.tmpdir} is {@code .} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link Name#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_whenFileResourceWithFIsPropertyIsJavaIoTmpdirIsDotToFile() {
    // Arrange
    Name name = new Name();
    name.setName("ant.regexp.regexpimpl");
    name.setProject(null);

    // Act and Assert
    assertFalse(name.isSelected(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), ".").toFile())));
  }

  /**
   * Test {@link Name#isSelected(Resource)}.
   * <ul>
   *   <li>When {@link FileResource#FileResource(File)} with f is Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link Name#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_whenFileResourceWithFIsPropertyIsJavaIoTmpdirIsTestTxtToFile() {
    // Arrange
    Name name = new Name();
    name.setName("ant.regexp.regexpimpl");
    name.setProject(null);

    // Act and Assert
    assertFalse(
        name.isSelected(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile())));
  }

  /**
   * Test {@link Name#isSelected(Resource)}.
   * <ul>
   *   <li>When {@link Resource#Resource(String)} with {@code Name}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Name#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_whenResourceWithName_thenReturnFalse() {
    // Arrange
    Name name = new Name();
    name.setName("ant.regexp.regexpimpl");
    name.setProject(null);

    // Act and Assert
    assertFalse(name.isSelected(new Resource("Name")));
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link Name}
   *   <li>{@link Name#setCaseSensitive(boolean)}
   *   <li>{@link Name#setHandleDirSep(boolean)}
   *   <li>{@link Name#setName(String)}
   *   <li>{@link Name#setProject(Project)}
   *   <li>{@link Name#getName()}
   *   <li>{@link Name#getRegex()}
   *   <li>{@link Name#isCaseSensitive()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    Name actualName = new Name();
    actualName.setCaseSensitive(true);
    actualName.setHandleDirSep(true);
    actualName.setName("foo");
    actualName.setProject(new Project());
    String actualName2 = actualName.getName();
    String actualRegex = actualName.getRegex();

    // Assert
    assertEquals("foo", actualName2);
    assertNull(actualRegex);
    assertTrue(actualName.isCaseSensitive());
  }
}
