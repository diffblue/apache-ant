package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import java.io.IOException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.resources.Resources;
import org.junit.Test;

public class ConcatResourceInputStreamDiffblueTest {
  /**
   * Test {@link ConcatResourceInputStream#read()}.
   * <ul>
   *   <li>Given {@link ConcatResourceInputStream#ConcatResourceInputStream(ResourceCollection)} with rc is {@link Resources#NONE}.</li>
   *   <li>Then return {@link Retryable#RETRY_FOREVER}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ConcatResourceInputStream#read()}
   */
  @Test
  public void testRead_givenConcatResourceInputStreamWithRcIsNone_thenReturnRetry_forever() throws IOException {
    // Arrange, Act and Assert
    assertEquals(Retryable.RETRY_FOREVER, (new ConcatResourceInputStream(Resources.NONE)).read());
  }

  /**
   * Test {@link ConcatResourceInputStream#read()}.
   * <ul>
   *   <li>Given {@link Path#Path(Project, String)} with p is {@link Project} (default constructor) and {@code Path}.</li>
   *   <li>Then return {@link Retryable#RETRY_FOREVER}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ConcatResourceInputStream#read()}
   */
  @Test
  public void testRead_givenPathWithPIsProjectAndPath_thenReturnRetry_forever() throws IOException {
    // Arrange, Act and Assert
    assertEquals(Retryable.RETRY_FOREVER, (new ConcatResourceInputStream(new Path(new Project(), "Path"))).read());
  }

  /**
   * Test {@link ConcatResourceInputStream#read()}.
   * <ul>
   *   <li>Given {@link Resource#Resource()} Exists is {@code false}.</li>
   *   <li>Then return {@link Retryable#RETRY_FOREVER}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ConcatResourceInputStream#read()}
   */
  @Test
  public void testRead_givenResourceExistsIsFalse_thenReturnRetry_forever() throws IOException {
    // Arrange
    Resource rc = new Resource();
    rc.setExists(false);

    // Act and Assert
    assertEquals(Retryable.RETRY_FOREVER, (new ConcatResourceInputStream(rc)).read());
  }
}
