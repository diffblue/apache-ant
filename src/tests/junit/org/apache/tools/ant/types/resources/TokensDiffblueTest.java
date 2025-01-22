package org.apache.tools.ant.types.resources;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.List;
import java.util.Set;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.filters.TokenFilter;
import org.apache.tools.ant.filters.TokenFilter.FileTokenizer;
import org.apache.tools.ant.taskdefs.Concat;
import org.apache.tools.ant.types.FileList;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.util.Tokenizer;
import org.junit.Test;

public class TokensDiffblueTest {
  /**
   * Test {@link Tokens#getCollection()}.
   * <p>
   * Method under test: {@link Tokens#getCollection()}
   */
  @Test
  public void testGetCollection() throws BuildException {
    // Arrange
    Tokens tokens = new Tokens();
    tokens.add(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));

    // Act and Assert
    assertThrows(BuildException.class, () -> tokens.getCollection());
  }

  /**
   * Test {@link Tokens#getCollection()}.
   * <p>
   * Method under test: {@link Tokens#getCollection()}
   */
  @Test
  public void testGetCollection2() throws BuildException {
    // Arrange
    Tokens tokens = new Tokens();
    tokens.add(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), "Name"));

    // Act
    Collection<Resource> actualCollection = tokens.getCollection();

    // Assert
    assertTrue(actualCollection instanceof List);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link Tokens#getCollection()}.
   * <p>
   * Method under test: {@link Tokens#getCollection()}
   */
  @Test
  public void testGetCollection3() throws BuildException {
    // Arrange
    Tokens tokens = new Tokens();
    tokens.add(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), ".").toFile()));

    // Act and Assert
    assertThrows(BuildException.class, () -> tokens.getCollection());
  }

  /**
   * Test {@link Tokens#getCollection()}.
   * <p>
   * Method under test: {@link Tokens#getCollection()}
   */
  @Test
  public void testGetCollection4() throws BuildException {
    // Arrange
    Tokens tokens = new Tokens();
    tokens.add(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "..").toFile()));

    // Act and Assert
    assertThrows(BuildException.class, () -> tokens.getCollection());
  }

  /**
   * Test {@link Tokens#getCollection()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) addFilelist {@link FileList#FileList()}.</li>
   *   <li>Then return {@link List}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tokens#getCollection()}
   */
  @Test
  public void testGetCollection_givenConcatAddFilelistFileList_thenReturnList() throws BuildException {
    // Arrange
    Concat c = new Concat();
    c.addFilelist(new FileList());

    Tokens tokens = new Tokens();
    tokens.add(c);

    // Act
    Collection<Resource> actualCollection = tokens.getCollection();

    // Assert
    assertTrue(actualCollection instanceof List);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link Tokens#getCollection()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Binary is {@code true}.</li>
   *   <li>Then return {@link List}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tokens#getCollection()}
   */
  @Test
  public void testGetCollection_givenConcatBinaryIsTrue_thenReturnList() throws BuildException {
    // Arrange
    Concat c = new Concat();
    c.setBinary(true);
    c.addFilelist(new FileList());

    Tokens tokens = new Tokens();
    tokens.add(c);

    // Act
    Collection<Resource> actualCollection = tokens.getCollection();

    // Assert
    assertTrue(actualCollection instanceof List);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link Tokens#getCollection()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) Dest is {@link Resource#Resource()}.</li>
   *   <li>Then return {@link List}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tokens#getCollection()}
   */
  @Test
  public void testGetCollection_givenConcatDestIsResource_thenReturnList() throws BuildException {
    // Arrange
    Concat c = new Concat();
    c.setDest(new Resource());
    c.addFilelist(new FileList());

    Tokens tokens = new Tokens();
    tokens.add(c);

    // Act
    Collection<Resource> actualCollection = tokens.getCollection();

    // Assert
    assertTrue(actualCollection instanceof List);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link Tokens#getCollection()}.
   * <ul>
   *   <li>Given {@link Concat} (default constructor) ResourceName is {@code Concatenating}.</li>
   *   <li>Then return {@link List}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tokens#getCollection()}
   */
  @Test
  public void testGetCollection_givenConcatResourceNameIsConcatenating_thenReturnList() throws BuildException {
    // Arrange
    Concat c = new Concat();
    c.setResourceName("Concatenating ");
    c.addFilelist(new FileList());

    Tokens tokens = new Tokens();
    tokens.add(c);

    // Act
    Collection<Resource> actualCollection = tokens.getCollection();

    // Assert
    assertTrue(actualCollection instanceof List);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link Tokens#getCollection()}.
   * <ul>
   *   <li>Given {@link Files#Files()} appendIncludes array of {@link String} with {@code **}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tokens#getCollection()}
   */
  @Test
  public void testGetCollection_givenFilesAppendIncludesArrayOfStringWithAsteriskAsterisk() throws BuildException {
    // Arrange
    Files c = new Files();
    c.appendIncludes(new String[]{"**"});

    Tokens tokens = new Tokens();
    tokens.add(c);

    // Act and Assert
    assertThrows(BuildException.class, () -> tokens.getCollection());
  }

  /**
   * Test {@link Tokens#getCollection()}.
   * <ul>
   *   <li>Given {@link Tokens} (default constructor) add {@link Archives} (default constructor).</li>
   *   <li>Then return {@link Set}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tokens#getCollection()}
   */
  @Test
  public void testGetCollection_givenTokensAddArchives_thenReturnSet() throws BuildException {
    // Arrange
    Tokens tokens = new Tokens();
    tokens.add(new Archives());

    // Act
    Collection<Resource> actualCollection = tokens.getCollection();

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link Tokens#getCollection()}.
   * <ul>
   *   <li>Given {@link Tokens} (default constructor) add {@link Files#Files()}.</li>
   *   <li>Then return {@link Set}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tokens#getCollection()}
   */
  @Test
  public void testGetCollection_givenTokensAddFiles_thenReturnSet() throws BuildException {
    // Arrange
    Tokens tokens = new Tokens();
    tokens.add(new Files());

    // Act
    Collection<Resource> actualCollection = tokens.getCollection();

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link Tokens#getCollection()}.
   * <ul>
   *   <li>Given {@link Tokens} (default constructor) add {@link Resources#NONE}.</li>
   *   <li>Then return {@link Set}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tokens#getCollection()}
   */
  @Test
  public void testGetCollection_givenTokensAddNone_thenReturnSet() throws BuildException {
    // Arrange
    Tokens tokens = new Tokens();
    tokens.add(Resources.NONE);

    // Act
    Collection<Resource> actualCollection = tokens.getCollection();

    // Assert
    assertTrue(actualCollection instanceof Set);
    assertTrue(actualCollection.isEmpty());
  }

  /**
   * Test {@link Tokens#add(Tokenizer)} with {@code tokenizer}.
   * <ul>
   *   <li>Given {@link Tokens} (default constructor) add {@link FileTokenizer} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Tokens#add(Tokenizer)}
   */
  @Test
  public void testAddWithTokenizer_givenTokensAddFileTokenizer_thenThrowBuildException() {
    // Arrange
    Tokens tokens = new Tokens();
    tokens.add(new FileTokenizer());

    // Act and Assert
    assertThrows(BuildException.class, () -> tokens.add(new FileTokenizer()));
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link Tokens}
   *   <li>{@link Tokens#setEncoding(String)}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    Tokens actualTokens = new Tokens();
    actualTokens.setEncoding("UTF-8");

    // Assert
    Location location = actualTokens.getLocation();
    assertNull(location.getFileName());
    assertNull(actualTokens.getDescription());
    assertNull(actualTokens.getProject());
    assertNull(actualTokens.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(actualTokens.isCache());
  }
}
